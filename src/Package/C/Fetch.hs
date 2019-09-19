{-# LANGUAGE OverloadedStrings #-}

module Package.C.Fetch ( fetchUrl
                       ) where

import           CPkgPrelude
import qualified Data.ByteString.Lazy    as BSL
import           Data.List               (isSuffixOf)
import           Data.Maybe              (fromJust)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.URI
import           Package.C.Db.Register
import           Package.C.Error
import           Package.C.Logging
import           Package.C.Monad
import           Package.C.Unpack
import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         (takeFileName)

urlToCompression :: MonadIO m => String -> m Compression
urlToCompression s | ".tar.gz" `isSuffixOf` s || ".tgz" `isSuffixOf` s = pure $ Tar Gz
                   | ".tar.xz" `isSuffixOf` s = pure $ Tar Xz
                   | ".tar.bz2" `isSuffixOf` s = pure $ Tar Bz2
                   | ".tar.lz" `isSuffixOf` s = pure $ Tar Lz
                   | ".tar" `isSuffixOf` s = pure $ Tar None
                   | ".zip" `isSuffixOf` s = pure Zip
                   | otherwise = unrecognized s

asFilename :: String -> Maybe String
asFilename = fmap (takeFileName . uriPath) . parseURI

cacheDir :: MonadIO m => m FilePath
cacheDir = (</> "cache") <$> globalPkgDir

fetchUrl :: String -- ^ URL
         -> String -- ^ Package name
         -> Bool -- ^ Stream using @tar@ package?
         -> FilePath -- ^ Directory to unpack to
         -> PkgM ()
fetchUrl url name libArch dirName = do

        let tarballName = fromJust (asFilename url)
        tarballDir <- (</> tarballName) <$> cacheDir
        shouldDownload <- not <$> liftIO (doesFileExist tarballDir)

        compression <- urlToCompression url

        response <-
            if shouldDownload
                then do
                    putNormal ("Downloading " ++ name)

                    putLoud ("from URL " ++ url)

                    manager <- liftIO $ newManager tlsManagerSettings
                    initialRequest <- liftIO $ parseRequest url
                    liftIO $ responseBody <$> httpLbs (initialRequest { method = "GET" }) manager
                else do
                    putDiagnostic ("Using cached tarball at " ++ tarballDir)
                    liftIO $ BSL.readFile tarballDir

        cacheDirExists <- liftIO (doesFileExist =<< cacheDir)
        unless cacheDirExists
            (liftIO $ createDirectoryIfMissing True =<< cacheDir)

        when shouldDownload $ do
            -- TODO: should cache/compress to .tar.xz?
            putLoud ("Caching " ++ tarballName)
            liftIO $ BSL.writeFile tarballDir response

        putNormal ("Unpacking " ++ name)

        liftIO $ unpackResponse compression libArch dirName response
