module Package.C.Build ( buildCPkg
                       ) where

import           Package.C.Type
import           System.IO.Temp (withSystemTempDirectory)

-- cPkgToDir :: CPkg -> IO FilePath
-- cPkgToDir cpkg = pure ""

configureInDir :: CPkg -> FilePath -> IO ()
configureInDir _ _ = mempty

buildInDir :: CPkg -> FilePath -> IO ()
buildInDir _ _ = mempty

-- https://mirrors.ocf.berkeley.edu/gnu/libunistring/libunistring-0.9.10.tar.xz
fetchCPkg :: CPkg
          -> FilePath -- ^ Directory for intermediate build files
          -> IO ()
fetchCPkg _ _ = mempty

-- TODO: more complicated solver, garbage collector, and all that.
-- Basically nix-style builds for C libraries
--
-- TODO: This should take a verbosity
-- TODO: play nicely with cross-compilation (lol)
buildCPkg :: CPkg -> IO ()
buildCPkg cpkg =

    withSystemTempDirectory "cpkg" $ \p -> do

        fetchCPkg cpkg p

        configureInDir cpkg p

        buildInDir cpkg p
