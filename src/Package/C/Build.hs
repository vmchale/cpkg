module Package.C.Build ( buildCPkg
                       ) where

import           Data.Foldable    (traverse_)
import           Package.C.Error
import           Package.C.Fetch
import           Package.C.Type
import           System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import           System.Exit      (ExitCode (ExitSuccess), exitWith)
import           System.FilePath  ((</>))
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (CreateProcess (cwd, std_in), StdStream (NoStream), createProcess, proc, waitForProcess)

handleExit :: ExitCode -> IO ()
handleExit ExitSuccess = mempty
handleExit x           = exitWith x

cPkgToDir :: CPkg -> IO FilePath
cPkgToDir cpkg = getAppUserDataDirectory (".cpkg" </> _pkgName cpkg)

stepToProc :: String -- ^ Step
           -> FilePath -- ^ Working directory
           -> IO CreateProcess
stepToProc s fp = case words s of
    x:xs -> pure $ (proc x xs) { cwd = Just fp, std_in = NoStream }
    _    -> badCommand

waitProcess :: CreateProcess -> IO ()
waitProcess proc' = do
    (_, _, _, r) <- createProcess proc'
    handleExit =<< waitForProcess r

configureInDir :: CPkg -> FilePath -> FilePath -> IO ()
configureInDir cpkg pkgDir _ = do

    createDirectoryIfMissing True pkgDir

    let cfg = ConfigureVars pkgDir []
        steps = _configureCommand cpkg cfg

    traverse_ waitProcess =<< traverse (stepToProc pkgDir) steps

buildInDir :: CPkg -> FilePath -> IO ()
buildInDir _ _ = mempty

-- https://mirrors.ocf.berkeley.edu/gnu/libunistring/libunistring-0.9.10.tar.xz
fetchCPkg :: CPkg
          -> FilePath -- ^ Directory for intermediate build files
          -> IO ()
fetchCPkg = fetchUrl . _pkgUrl

-- TODO: more complicated solver, garbage collector, and all that.
-- Basically nix-style builds for C libraries
--
-- TODO: This should take a verbosity
-- TODO: play nicely with cross-compilation (lol)
buildCPkg :: CPkg -> IO ()
buildCPkg cpkg = do

    pkgDir <- cPkgToDir cpkg

    withSystemTempDirectory "cpkg" $ \p -> do

        fetchCPkg cpkg p

        configureInDir cpkg pkgDir p

        buildInDir cpkg p
