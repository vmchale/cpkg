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
