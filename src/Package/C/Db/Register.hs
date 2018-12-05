module Package.C.Db.Register ( registerPkg
                             , cPkgToDir
                             , globalPkgDir
                             , printFlags
                             ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Binary            (decode, encode)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Foldable          (sequenceA_)
import           Data.Hashable          (Hashable (hash))
import qualified Data.Set               as S
import           Lens.Micro             (over)
import           Numeric                (showHex)
import           Package.C.Db.Type
import           Package.C.Error
import           Package.C.Type
import           Package.C.Type.Shared  hiding (Dep (name))
import           Package.C.Type.Version
import           System.Directory
import           System.FilePath        ((</>))

printFlags :: String -> IO ()
printFlags name = do

    maybePackage <- lookupPackage name

    case maybePackage of
        Nothing -> indexError name
        Just p -> sequenceA_ [ putStr "C flags: "
                             , putStrLn =<< buildCfgToCFlags p
                             , putStr "Linker flags: "
                             , putStrLn =<< buildCfgToLinkerFlags p
                             ]

-- TODO: do something more sophisticated; allow packages to return their own
-- dir?
buildCfgToLinkerFlags :: MonadIO m => BuildCfg -> m String
buildCfgToLinkerFlags = fmap (("-L" ++) . (</> "lib")) . buildCfgToDir

buildCfgToCFlags :: MonadIO m => BuildCfg -> m String
buildCfgToCFlags = fmap (("-I" ++) . (</> "include")) . buildCfgToDir

strictIndex :: MonadIO m => m InstallDb
strictIndex = do

    indexFile <- pkgIndex
    existsIndex <- liftIO (doesFileExist indexFile)

    if existsIndex
        then decode . BSL.fromStrict <$> liftIO (BS.readFile indexFile)
        else pure mempty

lookupPackage :: MonadIO m => String -> m (Maybe BuildCfg)
lookupPackage name = do

    indexContents <- strictIndex

    let matches = S.filter (\pkg -> buildName pkg == name) (_installedPackages indexContents)

    pure (S.lookupMax matches)

-- TODO: replace this with a proper/sensible database
-- Add some proper error handling here
registerPkg :: MonadIO m => CPkg -> m ()
registerPkg cpkg = do

    indexFile <- pkgIndex
    indexContents <- strictIndex

    let buildCfg = pkgToBuildCfg cpkg
        newIndex = over installedPackages (S.insert buildCfg) indexContents

    liftIO $ BSL.writeFile indexFile (encode newIndex)

pkgToBuildCfg :: CPkg -> BuildCfg
pkgToBuildCfg (CPkg n v _ _ _ _ _ _ _) =
    BuildCfg n v mempty mempty Nothing -- TODO: fix pinned build deps &c.

pkgIndex :: MonadIO m => m FilePath
pkgIndex = (</> "index.bin") <$> globalPkgDir

globalPkgDir :: MonadIO m => m FilePath
globalPkgDir = liftIO (getAppUserDataDirectory "cpkg")

platformString :: Maybe Platform -> (FilePath -> FilePath -> FilePath)
platformString Nothing  = (</>)
platformString (Just p) = \x y -> x </> p </> y

buildCfgToDir :: MonadIO m => BuildCfg -> m FilePath
buildCfgToDir buildCfg = do
    global <- globalPkgDir
    let hashed = showHex (hash buildCfg) mempty
        (<?>) = platformString (targetArch buildCfg)
    pure (global <?> buildName buildCfg ++ "-" ++ showVersion (buildVersion buildCfg) ++ "-" ++ hashed)

cPkgToDir :: MonadIO m => CPkg -> m FilePath
cPkgToDir = buildCfgToDir . pkgToBuildCfg
