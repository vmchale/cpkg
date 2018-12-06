module Package.C.Dhall ( getCPkg
                       , getPkgs
                       ) where

import           Dhall
import           Package.C.Dhall.Type
import           Package.C.Type.Verbosity

maybeMod :: Verbosity -> IO a -> IO a
maybeMod v | v >= Verbose = detailed
           | otherwise = id

getDhall :: Interpret a => Verbosity -> FilePath -> IO a
getDhall v = maybeMod v . inputFile auto

getCPkg :: Verbosity -> FilePath -> IO CPkg
getCPkg = getDhall

getPkgs :: Verbosity -> FilePath -> IO [CPkg]
getPkgs = getDhall
