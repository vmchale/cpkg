module Package.C.Db.Register ( registerPkg
                                   , cPkgToDir
                                   , globalPkgDir
                                   ) where

import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Binary             (decode, encode)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import           Data.Hashable           (Hashable (hash))
import qualified Data.Set                as S
import           Lens.Micro              (over)
import           Numeric                 (showHex)
import           Package.C.Db.Type
import           Package.C.Type
import           Package.C.Type.Version
import           System.Directory
import           System.FilePath         ((</>))

-- TODO: replace this with a proper/sensible database
-- Add some proper error handling here
registerPkg :: MonadIO m => CPkg -> m ()
registerPkg cpkg = do

    indexFile <- pkgIndex
    existsIndex <- liftIO (doesFileExist indexFile)

    indexContents <- if existsIndex
        then decode . BSL.fromStrict <$> liftIO (BS.readFile indexFile)
        else pure mempty

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

buildCfgToDir :: MonadIO m => BuildCfg -> m FilePath
buildCfgToDir buildCfg = do
    global <- globalPkgDir
    let hashed = showHex (hash buildCfg) mempty
    pure (global </> buildName buildCfg ++ "-" ++ showVersion (buildVersion buildCfg) ++ "-" ++ hashed)

cPkgToDir :: MonadIO m => CPkg -> m FilePath
cPkgToDir = buildCfgToDir . pkgToBuildCfg
