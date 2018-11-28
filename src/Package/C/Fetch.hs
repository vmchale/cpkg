{-# LANGUAGE OverloadedStrings #-}

module Package.C.Fetch ( fetchUrl
                       ) where

import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.List               (isSuffixOf)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Package.C.Error
import           Package.C.Monad
import           Package.C.Unpack

urlToCompression :: MonadIO m => String -> m Compression
urlToCompression s | ".tar.gz" `isSuffixOf` s || ".tgz" `isSuffixOf` s = pure $ Tar Gz
                   | ".tar.xz" `isSuffixOf` s = pure $ Tar Xz
                   | ".tar.bz2" `isSuffixOf` s = pure $ Tar Bz2
                   | ".tar" `isSuffixOf` s = pure $ Tar None
                   | ".zip" `isSuffixOf` s = pure Zip
                   | otherwise = unrecognized s

fetchUrl :: String -- ^ URL
         -> String -- ^ Package name
         -> FilePath -- ^ Directory to unpack to
         -> PkgM ()
fetchUrl url name dirName = do

        compression <- urlToCompression url

        putNormal ("Downloading " ++ name)

        manager <- liftIO $ newManager tlsManagerSettings
        initialRequest <- liftIO $ parseRequest url
        response <- liftIO $ responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        putNormal ("Unpacking " ++ name)

        liftIO $ unpackResponse compression dirName response
