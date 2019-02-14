module Package.C.Unpack ( unpackResponse
                        , Compression (..)
                        , TarCompress (..)
                        ) where

import qualified Codec.Archive.Tar      as Tar
import           Codec.Archive.Zip      (ZipOption (..), extractFilesFromArchive, toArchive)
import qualified Codec.Compression.BZip as Bzip
import qualified Codec.Compression.GZip as Gzip
import qualified Codec.Compression.Lzma as Lzma
import qualified Data.ByteString.Lazy   as BSL
import           System.Directory

data TarCompress = Gz
                 | Xz
                 | Bz2
                 | None

data Compression = Tar TarCompress
                 | Zip


getCompressor :: TarCompress -> BSL.ByteString -> BSL.ByteString
getCompressor Gz   = Gzip.decompress
getCompressor None = id
getCompressor Xz   = Lzma.decompress
getCompressor Bz2  = Bzip.decompress

tarResponse :: TarCompress -> FilePath -> BSL.ByteString -> IO ()
tarResponse compressScheme dirName response =
    let f = Tar.unpack dirName . Tar.read . getCompressor compressScheme
    in f response

zipResponse :: FilePath -> BSL.ByteString -> IO ()
zipResponse dirName response = withCurrentDirectory dirName $ do
    let options = OptDestination dirName
    extractFilesFromArchive [options] (toArchive response)

unpackResponse :: Compression -> FilePath -> BSL.ByteString -> IO ()
unpackResponse (Tar tarCmp) fp response = tarResponse tarCmp fp response
unpackResponse Zip fp response          = zipResponse fp response
