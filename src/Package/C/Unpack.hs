module Package.C.Unpack ( unpackResponse
                        , Compression (..)
                        , TarCompress (..)
                        ) where

import qualified Codec.Archive          as Archive
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

archiveResponse :: TarCompress -> FilePath -> BSL.ByteString -> IO ()
archiveResponse compressScheme dirName =
    Archive.unpackToDir dirName . BSL.toStrict . getCompressor compressScheme

tarResponse :: TarCompress -> FilePath -> BSL.ByteString -> IO ()
tarResponse compressScheme dirName =
    Tar.unpack dirName . Tar.read . getCompressor compressScheme

zipResponse :: FilePath -> BSL.ByteString -> IO ()
zipResponse dirName response = withCurrentDirectory dirName $ do
    let options = OptDestination dirName
    extractFilesFromArchive [options] (toArchive response)

unpackResponse :: Compression -> Bool -> FilePath -> BSL.ByteString -> IO ()
unpackResponse (Tar tarCmp) True fp response  = tarResponse tarCmp fp response
unpackResponse (Tar tarCmp) False fp response = archiveResponse tarCmp fp response
unpackResponse Zip _ fp response              = zipResponse fp response
