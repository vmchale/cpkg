{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- TODO: a lot of the stuff in this module could be made pure so that it only
-- gets called once
module Package.C.Db.Register ( registerPkg
                             , unregisterPkg
                             , uninstallPkg
                             , uninstallPkgByName
                             , installedDb
                             , lookupOrFail
                             , cPkgToDir
                             , globalPkgDir
                             , printCompilerFlags
                             , printLinkerFlags
                             , printPkgConfigPath
                             , printIncludePath
                             , printLibPath
                             , printCabalFlags
                             , printLdLibPath
                             , packageInstalled
                             , allPackages
                             , parseHostIO
                             , Platform
                             ) where

import           Control.Monad.Reader
import           Control.Monad.State  (modify)
import           CPkgPrelude
import           Data.Binary          (encode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable        (Hashable (..))
import           Data.List            (intercalate)
import qualified Data.Set             as S
import           Numeric              (showHex)
import           Package.C.Db.Memory
import           Package.C.Db.Monad
import           Package.C.Db.Type
import           Package.C.Error
import           Package.C.Logging
import           Package.C.Triple
import           Package.C.Type       hiding (Dep (name))

type Platform = String
type FlagPrint = forall m. MonadIO m => BuildCfg -> m String

allPackages :: IO [String]
allPackages = do
    (InstallDb index) <- strictIndex
    pure (buildName <$> toList index)

printCompilerFlags :: (MonadIO m, MonadDb m) => String -> Maybe Platform -> m ()
printCompilerFlags = printFlagsWith buildCfgToCFlags

printLinkerFlags :: (MonadIO m, MonadDb m) => String -> Maybe Platform -> m ()
printLinkerFlags = printFlagsWith buildCfgToLinkerFlags

printPkgConfigPath :: (MonadIO m, MonadDb m) => [String] -> Maybe Platform -> m ()
printPkgConfigPath = printMany (liftIO . putStrLn <=< (fmap (intercalate ":") . traverse buildCfgToPkgConfigPath))

printIncludePath :: (MonadIO m, MonadDb m) => String -> Maybe Platform -> m ()
printIncludePath = printFlagsWith buildCfgToIncludePath

printLibPath :: (MonadIO m, MonadDb m) => String -> Maybe Platform -> m ()
printLibPath = printFlagsWith buildCfgToLibPath

parseHostIO :: MonadIO m => Maybe Platform -> m (Maybe TargetTriple)
parseHostIO (Just x) = fmap Just (parseTripleIO x)
parseHostIO Nothing  = pure Nothing

printFlagsWith :: (MonadIO m, MonadDb m) => FlagPrint -> String -> Maybe Platform -> m ()
printFlagsWith f name host = do

    parsedHost <- parseHostIO host

    maybePackage <- lookupPackage name parsedHost

    case maybePackage of
        Nothing -> indexError name
        Just p  -> liftIO (putStrLn =<< f p)

printMany :: (MonadIO m, MonadDb m) => ([BuildCfg] -> m ()) -> [String] -> Maybe Platform -> m ()
printMany f names host = do

    parsedHost <- parseHostIO host

    maybePackages <- sequenceA <$> traverse (\n -> lookupPackage n parsedHost) names

    case maybePackages of
        Nothing -> indexError (head names)
        Just ps -> f ps

printLdLibPath :: (MonadIO m, MonadDb m) => [String] -> Maybe Platform -> m ()
printLdLibPath = printMany (liftIO . putStrLn <=< (fmap (intercalate ":") . traverse buildCfgToLibPath))

printCabalFlags :: (MonadIO m, MonadDb m) => [String] -> Maybe Platform -> m ()
printCabalFlags = printMany (liftIO . putStrLn <=< (fmap unwords . traverse buildCfgToCabalFlag))

buildCfgToCabalFlag :: MonadIO m => BuildCfg -> m String
buildCfgToCabalFlag = fmap (("--extra-lib-dirs=" ++) . (</> "lib")) . buildCfgToDir

-- TODO: do something more sophisticated; allow packages to return their own
-- dir?
buildCfgToLinkerFlags :: MonadIO m => BuildCfg -> m String
buildCfgToLinkerFlags = fmap (("-L" ++) . (</> "lib")) . buildCfgToDir

buildCfgToCFlags :: MonadIO m => BuildCfg -> m String
buildCfgToCFlags = fmap (("-I" ++) . (</> "include")) . buildCfgToDir

buildCfgToPkgConfigPath :: MonadIO m => BuildCfg -> m String
buildCfgToPkgConfigPath = fmap (</> "lib" </> "pkgconfig") . buildCfgToDir

buildCfgToLibPath :: MonadIO m => BuildCfg -> m String
buildCfgToLibPath = fmap (</> "lib") . buildCfgToDir

buildCfgToIncludePath :: MonadIO m => BuildCfg -> m String
buildCfgToIncludePath = fmap (</> "include") . buildCfgToDir

installedDb :: (MonadIO m, MonadDb m)
                  => m (S.Set BuildCfg)
installedDb =
    _installedPackages <$> memIndex

packageInstalled :: (MonadIO m, MonadDb m)
                 => CPkg
                 -> Maybe TargetTriple
                 -> Bool
                 -> BuildVars
                 -> m Bool
packageInstalled pkg host glob b = do

    packs <- installedDb

    pure $
           (pkgToBuildCfg pkg host glob True b `S.member` packs)
        || (pkgToBuildCfg pkg host glob False b `S.member` packs)

lookupPackage :: (MonadIO m, MonadDb m) => String -> Maybe TargetTriple -> m (Maybe BuildCfg)
lookupPackage name host = do

    packs <- installedDb

    let matches = S.filter (\pkg -> buildName pkg == name && targetArch pkg == host) packs

    pure (S.lookupMax matches)

lookupOrFail :: (MonadIO m, MonadDb m) => String -> Maybe TargetTriple -> m BuildCfg
lookupOrFail name host = do
    pk <- lookupPackage name host
    case pk of
        Just cfg -> pure cfg
        Nothing  -> notInstalled name

-- | @since 0.2.3.0
uninstallPkgByName :: (MonadReader Verbosity m, MonadIO m, MonadDb m)
                   => String
                   -> Maybe TargetTriple
                   -> m ()
uninstallPkgByName name host =
    uninstallPkg =<< lookupOrFail name host

uninstallPkg :: (MonadIO m, MonadDb m, MonadReader Verbosity m)
             => BuildCfg
             -> m ()
uninstallPkg cpkg = do
    unregisterPkg cpkg
    (liftIO . removeDirectoryRecursive)
        =<< buildCfgToDir cpkg

unregisterPkg :: (MonadIO m, MonadDb m, MonadReader Verbosity m)
              => BuildCfg
              -> m ()
unregisterPkg buildCfg = do

    putDiagnostic ("Unregistering package " ++ buildName buildCfg ++ "...")

    indexFile <- pkgIndex
    indexContents <- memIndex

    let modIndex = over installedPackages (S.delete buildCfg)
        newIndex = modIndex indexContents

    modify modIndex

    liftIO $ BSL.writeFile indexFile (encode newIndex)

-- TODO: replace this with a proper/sensible database
registerPkg :: (MonadIO m, MonadDb m, MonadReader Verbosity m)
            => CPkg
            -> Maybe TargetTriple
            -> Bool -- ^ Globally installed?
            -> Bool -- ^ Manually installed?
            -> BuildVars
            -> m ()
registerPkg cpkg host glob usr b = do

    putDiagnostic ("Registering package " ++ pkgName cpkg ++ "...")

    indexFile <- pkgIndex
    indexContents <- memIndex

    let buildCfg = pkgToBuildCfg cpkg host glob usr b
        modIndex = over installedPackages (S.insert buildCfg)
        newIndex = modIndex indexContents

    modify modIndex

    liftIO $ BSL.writeFile indexFile (encode newIndex)

pkgToBuildCfg :: CPkg
              -> Maybe TargetTriple
              -> Bool
              -> Bool -- ^ Was this package manually installed?
              -> BuildVars
              -> BuildCfg
pkgToBuildCfg (CPkg n v _ _ _ _ _ cCmd bCmd iCmd) host glob usr bVar =
    BuildCfg n v mempty mempty host glob (cCmd bVar) (bCmd bVar) (iCmd bVar) usr -- TODO: fix pinned build deps &c.

platformString :: Maybe TargetTriple -> (FilePath -> FilePath -> FilePath)
platformString Nothing  = (</>)
platformString (Just p) = \x y -> x </> show p </> y

buildCfgToDir :: MonadIO m => BuildCfg -> m FilePath
buildCfgToDir buildCfg = do
    global' <- globalPkgDir
    -- when hashing, pretend everything has was NOT manually installed so they
    -- all have the same hash
    let hashed = showHex (abs (hash (buildCfg { manual = False}))) mempty
        (<?>) = platformString (targetArch buildCfg)
    pure (global' <?> buildName buildCfg ++ "-" ++ showVersion (buildVersion buildCfg) ++ "-" ++ hashed)

globDir :: Maybe TargetTriple -> FilePath
globDir Nothing      = "/usr/local"
globDir (Just arch') = "/usr" </> show arch'

cPkgToDir :: MonadIO m
          => CPkg
          -> Maybe TargetTriple
          -> Bool
          -> BuildVars
          -> m FilePath
cPkgToDir pk host False bv = buildCfgToDir (pkgToBuildCfg pk host False undefined bv)
cPkgToDir _ host _ _       = pure (globDir host)
