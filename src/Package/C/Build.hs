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

fetchCPkg :: CPkg
          -> FilePath -- ^ Directory for intermediate build files
          -> IO ()
fetchCPkg _ _ = mempty

-- TODO: This should take a verbosity
-- TODO: play nicely with cross-compilation (lol)
buildCPkg :: CPkg -> IO ()
buildCPkg cpkg =

    withSystemTempDirectory "cpkg" $ \p -> do

        fetchCPkg cpkg p

        configureInDir cpkg p

        buildInDir cpkg p
