{-# LANGUAGE FlexibleContexts #-}

module Package.C.Db.Memory ( strictIndex
                           , pkgIndex
                           , globalPkgDir
                           , memIndex
                           ) where

import           Control.Monad.State.Class
import           CPkgPrelude
import           Data.Binary               (decode)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Package.C.Db.Monad
import           Package.C.Db.Type

pkgIndex :: MonadIO m => m FilePath
pkgIndex = (</> "index.bin") <$> globalPkgDir

globalPkgDir :: MonadIO m => m FilePath
globalPkgDir = liftIO (getAppUserDataDirectory "cpkg")

memIndex :: MonadDb m => m InstallDb
memIndex = get

strictIndex :: MonadIO m => m InstallDb
strictIndex = do

    indexFile <- pkgIndex
    -- Add some proper error handling here
    existsIndex <- liftIO (doesFileExist indexFile)

    if existsIndex
        then decode . BSL.fromStrict <$> liftIO (BS.readFile indexFile)
        else pure mempty
