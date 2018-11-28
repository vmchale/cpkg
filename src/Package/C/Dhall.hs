module Package.C.Dhall ( getCPkg
                       ) where

import           Dhall                (auto, inputFile)
import           Package.C.Dhall.Type

getCPkg :: FilePath -> IO CPkg
getCPkg = inputFile auto
