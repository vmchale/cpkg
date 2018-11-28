module Main (main) where

import           Dhall     (auto, inputFile)
import           Package.C

main :: IO ()
main = do
    unistring <- cPkgDhallToCPkg <$> inputFile auto "examples/gc.dhall"
    runPkgM Loud (buildCPkg unistring)
