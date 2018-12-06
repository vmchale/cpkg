module Package.C.Build ( buildCPkg
                       ) where

import           Control.Concurrent          (getNumCapabilities)
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Foldable               (traverse_)
import           Package.C.Build.OS
import           Package.C.Db.Register
import           Package.C.Fetch
import           Package.C.Monad
import           Package.C.Type
import           System.Directory
import           System.Directory.Executable (mkExecutable)
import           System.FilePath             ((</>))
import           System.IO.Temp              (withSystemTempDirectory)
import           System.Process
import           System.Process.Ext

envVarSplit :: EnvVar -> (String, String)
envVarSplit (EnvVar ev x) = (ev, x)

stepToProc :: FilePath -- ^ Package directory
           -> Command
           -> PkgM ()
stepToProc fp (Call p as envs dir') = do
    let dir'' = maybe fp (fp </>) dir'
        envVars = fmap envVarSplit <$> envs
    putDiagnostic ("Running " ++ p ++ " with arguments " ++ unwords as ++ " in directory " ++ dir'' ++ " with environment " ++ show envVars)
    waitProcess $ (proc p as) { cwd = Just dir'', std_in = CreatePipe, env = envVars }
stepToProc dir' (MakeExecutable fp) = do
    putDiagnostic ("Marking " ++ (dir' </> fp) ++ " as executable...")
    liftIO $ mkExecutable (dir' </> fp)
stepToProc dir' (CreateDirectory d) = do
    putDiagnostic ("Creating directory " ++ (dir' </> d) ++ "...")
    liftIO $ createDirectoryIfMissing True (dir' </> d)
stepToProc _ SymlinkBinary{} = putDiagnostic "TODO: SymlinkBinary case"

processSteps :: (Traversable t) => FilePath -> t Command -> PkgM ()
processSteps pkgDir = traverse_ (stepToProc pkgDir)

configureInDir :: CPkg -> ConfigureVars -> FilePath -> PkgM ()
configureInDir cpkg cfg p =

    let steps = configureCommand cpkg cfg
    in
        putNormal ("Configuring " ++ pkgName cpkg) *>
        processSteps p steps

buildInDir :: CPkg -> BuildVars -> FilePath -> PkgM ()
buildInDir cpkg cfg p = do
    putNormal ("Building " ++ pkgName cpkg)
    processSteps p (buildCommand cpkg cfg)

installInDir :: CPkg -> InstallVars -> FilePath -> PkgM ()
installInDir cpkg cfg p =
    putNormal ("Installing " ++ pkgName cpkg) *>
    processSteps p (installCommand cpkg cfg)

fetchCPkg :: CPkg
          -> FilePath -- ^ Directory for intermediate build files
          -> PkgM ()
fetchCPkg cpkg = fetchUrl (pkgUrl cpkg) (pkgName cpkg)

-- diagnosticDirectory :: String -> (FilePath -> m a) -> m a
-- diagnosticDirectory s f = f (s ++ "-diagnostic")

buildCPkg :: CPkg
          -> Maybe Platform
          -> [FilePath] -- ^ Library directories
          -> [FilePath] -- ^ Include directories
          -> PkgM ()
buildCPkg cpkg host libs incls = do

    (configureVars, buildVars, installVars) <- getVars host libs incls

    installed <- packageInstalled cpkg host configureVars buildVars installVars

    unless installed $
        forceBuildCPkg cpkg host configureVars buildVars installVars

-- only really suitable for hashing at this point, since we use @""@ as the
-- install directory. we use this to get a hash which we then use to get the
-- *real* install directory, which we then use with @configureVars@ to set
-- things up correctly - otherwise we would have a circularity
getVars :: Maybe Platform
        -> [FilePath] -- ^ Library directories
        -> [FilePath] -- ^ Include directories
        -> PkgM (ConfigureVars, BuildVars, InstallVars)
getVars host incls links = do
    nproc <- liftIO getNumCapabilities
    let configureVars = ConfigureVars "" host incls links dhallOS
        buildVars = BuildVars nproc dhallOS
        installVars = InstallVars dhallOS
    pure (configureVars, buildVars, installVars)

-- TODO: more complicated solver, garbage collector, and all that.
-- Basically nix-style builds for C libraries
forceBuildCPkg :: CPkg
               -> Maybe Platform
               -> ConfigureVars
               -> BuildVars
               -> InstallVars
               -> PkgM ()
forceBuildCPkg cpkg host configureVars buildVars installVars = do

    pkgDir <- cPkgToDir cpkg host configureVars buildVars installVars

    withSystemTempDirectory "cpkg" $ \p -> do

        putDiagnostic ("Setting up temporary directory in " ++ p)

        fetchCPkg cpkg p

        let p' = p </> pkgSubdir cpkg

        configureInDir cpkg (configureVars { installDir = pkgDir }) p'

        buildInDir cpkg buildVars p'

        liftIO $ createDirectoryIfMissing True pkgDir

        installInDir cpkg installVars p'

        registerPkg cpkg host configureVars buildVars installVars
