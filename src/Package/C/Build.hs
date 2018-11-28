module Package.C.Build ( buildCPkg
                       , globalPkgDir
                       ) where

import           Control.Concurrent     (getNumCapabilities)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Foldable          (traverse_)
import           Package.C.Error
import           Package.C.Fetch
import           Package.C.Monad
import           Package.C.Type
import           Package.C.Version
import           System.Directory
import           System.FilePath        ((</>))
import           System.IO.Temp         (withSystemTempDirectory)
import           System.Process
import           System.Process.Ext

mkExecutable :: FilePath -> IO ()
mkExecutable fp = do
    perms <- getPermissions fp
    setPermissions fp (setOwnerExecutable True perms)

globalPkgDir :: MonadIO m => m FilePath
globalPkgDir = liftIO (getAppUserDataDirectory "cpkg")

cPkgToDir :: MonadIO m => CPkg -> m FilePath
cPkgToDir cpkg = do
    global <- globalPkgDir
    pure (global </> pkgName cpkg ++ "-" ++ showVersion (pkgVersion cpkg))

stepToProc :: MonadIO m
           => FilePath -- ^ Working directory
           -> String -- ^ Steo
           -> m CreateProcess
stepToProc fp s = case words s of
    x:xs -> pure $ (proc x xs) { cwd = Just fp, std_in = CreatePipe }
    _    -> badCommand

processSteps :: (Traversable t) => FilePath -> t String -> PkgM ()
processSteps pkgDir steps = traverse_ waitProcess =<< traverse (stepToProc pkgDir) steps

configureInDir :: CPkg -> FilePath -> FilePath -> PkgM ()
configureInDir cpkg pkgDir p =

    let cfg = ConfigureVars pkgDir Nothing []
        steps = configureCommand cpkg cfg
    in
        putNormal ("Configuring " ++ pkgName cpkg) *>
        processSteps p steps

buildInDir :: CPkg -> FilePath -> PkgM ()
buildInDir cpkg p = do
    cpus <- liftIO getNumCapabilities
    putNormal ("Building " ++ pkgName cpkg)
    let cfg = BuildVars (fromIntegral cpus)
    processSteps p (buildCommand cpkg cfg)

installInDir :: CPkg -> FilePath -> PkgM ()
installInDir cpkg p =
    putNormal ("Installing " ++ pkgName cpkg) *>
    processSteps p (installCommand cpkg)

fetchCPkg :: CPkg
          -> FilePath -- ^ Directory for intermediate build files
          -> PkgM ()
fetchCPkg cpkg = fetchUrl (pkgUrl cpkg) (pkgName cpkg)

-- TODO: more complicated solver, garbage collector, and all that.
-- Basically nix-style builds for C libraries
--
-- TODO: This should take a verbosity
-- TODO: play nicely with cross-compilation (lol)
buildCPkg :: CPkg -> PkgM ()
buildCPkg cpkg = do

    pkgDir <- cPkgToDir cpkg

    liftIO $ createDirectoryIfMissing True pkgDir

    -- FIXME: can't use withSystemTempDirectory for... reasons
    withSystemTempDirectory "cpkg" $ \p -> do

        putDiagnostic ("Setting up temporary directory in " ++ p)

        fetchCPkg cpkg p

        let p' = p </> pkgSubdir cpkg
            toExes = (p' </>) <$> executableFiles cpkg

        liftIO $ traverse_ mkExecutable toExes

        configureInDir cpkg pkgDir p'

        buildInDir cpkg p'

        installInDir cpkg p'
