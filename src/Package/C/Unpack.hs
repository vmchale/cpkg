module Package.C.Unpack ( unpackResponse
                        , Compression (..)
                        , TarCompress (..)
                        ) where

import qualified Codec.Archive               as Archive
import           Codec.Archive.Zip           (ZipOption (..), extractFilesFromArchive, toArchive)
import qualified Codec.Compression.BZip      as Bzip
import qualified Codec.Compression.GZip      as Gzip
import qualified Codec.Compression.Lzma      as Lzma
import qualified Codec.Compression.Zstd.Lazy as Zstd
import qualified Codec.Lzip                  as Lzip
import qualified Data.ByteString.Lazy        as BSL
import           System.Directory

data TarCompress = Gz
                 | Xz
                 | Bz2
                 | Lz
                 | Zstd
                 | None

data Compression = Tar TarCompress
                 | Zip


getCompressor :: TarCompress -> BSL.ByteString -> BSL.ByteString
getCompressor Gz   = Gzip.decompress
getCompressor None = id
getCompressor Xz   = Lzma.decompress
getCompressor Bz2  = Bzip.decompress
getCompressor Lz   = Lzip.decompress
getCompressor Zstd = Zstd.decompress

archiveResponse :: TarCompress -> FilePath -> BSL.ByteString -> IO ()
archiveResponse compressScheme dirName =
    fmap (either showError id) . Archive.runArchiveM . Archive.unpackToDirLazy dirName . getCompressor compressScheme

    where showError = error . show

zipResponse :: FilePath -> BSL.ByteString -> IO ()
zipResponse dirName response = withCurrentDirectory dirName $ do
    let options = OptDestination dirName
    extractFilesFromArchive [options] (toArchive response)

unpackResponse :: Compression -> FilePath -> BSL.ByteString -> IO ()
unpackResponse (Tar tarCmp) fp response = archiveResponse tarCmp fp response
unpackResponse Zip fp response          = zipResponse fp response
