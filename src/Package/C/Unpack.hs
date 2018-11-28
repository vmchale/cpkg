module Package.C.Unpack ( unpackResponse ) where

import qualified Data.ByteString.Lazy as BSL

type Unpacker = FilePath -> BSL.ByteString -> IO ()

unpackResponse :: Unpacker
unpackResponse _ _ = mempty
