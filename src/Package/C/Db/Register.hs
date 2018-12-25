{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- TODO: a lot of the stuff in this module could be made pure so that it only
-- gets called once
module Package.C.Db.Register ( registerPkg
                             , cPkgToDir
                             , globalPkgDir
                             , printCompilerFlags
                             , printLinkerFlags
                             , printPkgConfigPath
                             , printIncludePath
                             , printLibPath
                             , packageInstalled
                             , allPackages
                             ) where

import           Control.Monad.Reader
import           Control.Monad.State  (modify)
import           CPkgPrelude
import           Data.Binary          (encode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable        (Hashable (hash))
import qualified Data.Set             as S
import           Numeric              (showHex)
import           Package.C.Db.Memory
import           Package.C.Db.Monad
import           Package.C.Db.Type
import           Package.C.Error
import           Package.C.Logging
import           Package.C.Type       hiding (Dep (name))

type FlagPrint = forall m. MonadIO m => BuildCfg -> m String

allPackages :: IO [String]
allPackages = do
    (InstallDb index) <- strictIndex
    pure (buildName <$> toList index)

printCompilerFlags :: (MonadIO m, MonadDb m) => String -> Maybe String -> m ()
printCompilerFlags = printFlagsWith buildCfgToCFlags

printLinkerFlags :: (MonadIO m, MonadDb m) => String -> Maybe String -> m ()
printLinkerFlags = printFlagsWith buildCfgToLinkerFlags

printPkgConfigPath :: (MonadIO m, MonadDb m) => String -> Maybe String -> m ()
printPkgConfigPath = printFlagsWith buildCfgToPkgConfigPath

printIncludePath :: (MonadIO m, MonadDb m) => String -> Maybe String -> m ()
printIncludePath = printFlagsWith buildCfgToIncludePath

printLibPath :: (MonadIO m, MonadDb m) => String -> Maybe String -> m ()
printLibPath = printFlagsWith buildCfgToLibPath

printFlagsWith :: (MonadIO m, MonadDb m) => FlagPrint -> String -> Maybe String -> m ()
printFlagsWith f name host = do

    maybePackage <- lookupPackage name host

    case maybePackage of
        Nothing -> indexError name
        Just p  -> liftIO (putStrLn =<< f p)

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

packageInstalled :: (MonadIO m, MonadDb m)
                 => CPkg
                 -> Maybe Platform
                 -> BuildVars
                 -> m Bool
packageInstalled pkg host b = do

    indexContents <- memIndex

    pure (pkgToBuildCfg pkg host b `S.member` _installedPackages indexContents)

lookupPackage :: (MonadIO m, MonadDb m) => String -> Maybe Platform -> m (Maybe BuildCfg)
lookupPackage name host = do

    indexContents <- memIndex

    let matches = S.filter (\pkg -> buildName pkg == name && targetArch pkg == host) (_installedPackages indexContents)

    pure (S.lookupMax matches)

-- TODO: replace this with a proper/sensible database
registerPkg :: (MonadIO m, MonadDb m, MonadReader Verbosity m)
            => CPkg
            -> Maybe Platform
            -> BuildVars
            -> m ()
registerPkg cpkg host b = do

    putDiagnostic ("Registering package " ++ pkgName cpkg ++ "...")

    indexFile <- pkgIndex
    indexContents <- memIndex

    let buildCfg = pkgToBuildCfg cpkg host b
        modIndex = over installedPackages (S.insert buildCfg)
        newIndex = modIndex indexContents

    modify modIndex

    liftIO $ BSL.writeFile indexFile (encode newIndex)

pkgToBuildCfg :: CPkg
              -> Maybe Platform
              -> BuildVars
              -> BuildCfg
pkgToBuildCfg (CPkg n v _ _ _ _ cCmd bCmd iCmd) host bVar =
    BuildCfg n v mempty mempty host (cCmd bVar) (bCmd bVar) (iCmd bVar) -- TODO: fix pinned build deps &c.

platformString :: Maybe Platform -> (FilePath -> FilePath -> FilePath)
platformString Nothing  = (</>)
platformString (Just p) = \x y -> x </> p </> y

buildCfgToDir :: MonadIO m => BuildCfg -> m FilePath
buildCfgToDir buildCfg = do
    global <- globalPkgDir
    let hashed = showHex (abs (hash buildCfg)) mempty
        (<?>) = platformString (targetArch buildCfg)
    pure (global <?> buildName buildCfg ++ "-" ++ showVersion (buildVersion buildCfg) ++ "-" ++ hashed)

cPkgToDir :: MonadIO m
          => CPkg
          -> Maybe Platform
          -> BuildVars
          -> m FilePath
cPkgToDir = buildCfgToDir .** pkgToBuildCfg
