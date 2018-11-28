module Main (main) where

import           Dhall     (auto, inputFile)
import           Package.C

main :: IO ()
main = do
    unistring <- cPkgDhallToCPkg <$> inputFile auto "examples/unistring.dhall"
    runPkgM Loud (buildCPkg unistring)
