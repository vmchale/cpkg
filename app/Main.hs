module Main (main) where

import           Dhall     (auto, inputFile)
import           Package.C

main :: IO ()
main = do
    unistring <- cPkgDhallToCPkg <$> inputFile auto "examples/curl.dhall"
    runPkgM Loud (buildCPkg unistring)
