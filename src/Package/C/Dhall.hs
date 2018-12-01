module Package.C.Dhall ( getCPkg
                       ) where

import           Dhall                    (auto, detailed, inputFile)
import           Package.C.Dhall.Type
import           Package.C.Type.Verbosity

maybeMod :: Verbosity -> IO a -> IO a
maybeMod v | v >= Verbose = detailed
           | otherwise = id

getCPkg :: Verbosity -> FilePath -> IO CPkg
getCPkg v = maybeMod v . inputFile auto
