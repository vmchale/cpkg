{-# LANGUAGE OverloadedStrings #-}

module Package.C.Fetch ( fetchUrl
                       ) where

import           Data.List               (isSuffixOf)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Package.C.Error
import           Package.C.Unpack

urlToCompression :: String -> IO Compression
urlToCompression s | ".tar.gz" `isSuffixOf` s || ".tgz" `isSuffixOf` s = pure $ Tar Gz
                   | ".tar.xz" `isSuffixOf` s = pure $ Tar Xz
                   | ".tar.bz2" `isSuffixOf` s = pure $ Tar Bz2
                   | ".tar" `isSuffixOf` s = pure $ Tar None
                   | ".zip" `isSuffixOf` s = pure Zip
                   | otherwise = unrecognized s

fetchUrl :: String -- ^ URL
         -> FilePath -- ^ Directory to unpack to
         -> IO ()
fetchUrl url dirName = do

        compression <- urlToCompression url

        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest url
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        unpackResponse compression dirName response
