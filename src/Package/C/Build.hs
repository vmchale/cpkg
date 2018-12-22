module Package.C.Build ( buildCPkg
                       , getVars
                       , cPkgToDir
                       ) where

import           Control.Concurrent          (getNumCapabilities)
import           CPkgPrelude
import qualified Data.Text.IO                as TIO
import           Package.C.Build.OS
import           Package.C.Db.Register
import           Package.C.Fetch
import           Package.C.Monad
import           Package.C.Type
import           System.Directory
import           System.Directory.Executable (mkExecutable)
import           System.FilePath             (takeDirectory, takeFileName, (</>))
import           System.FilePath.Glob
import           System.IO.Temp              (withSystemTempDirectory)
import           System.Process
import           System.Process.Ext

envVarSplit :: EnvVar -> (String, String)
envVarSplit (EnvVar ev x) = (ev, x)

stepToProc :: FilePath -- ^ Package build directory
           -> FilePath -- ^ Package install directory
           -> Command
           -> PkgM ()
stepToProc fp _ (Call p as envs dir') = do
    let dir'' = maybe fp (fp </>) dir'
        envVars = fmap envVarSplit <$> envs
    putDiagnostic ("Running " ++ p ++ " with arguments " ++ unwords as ++ " in directory " ++ dir'' ++ " with environment " ++ show envVars)
    waitProcess $ (proc p as) { cwd = Just dir'', std_in = CreatePipe, env = envVars }
stepToProc dir' _ (MakeExecutable fp) = do
    putDiagnostic ("Marking " ++ (dir' </> fp) ++ " as executable...")
    liftIO $ mkExecutable (dir' </> fp)
stepToProc dir' _ (CreateDirectory d) = do
    putDiagnostic ("Creating directory " ++ (dir' </> d) ++ "...")
    liftIO $ createDirectoryIfMissing True (dir' </> d)
stepToProc _ p (SymlinkBinary file') = do
    binDir <- (</> "bin") <$> globalPkgDir
    let actualBin = p </> file'
    liftIO $ createDirectoryIfMissing True binDir
    liftIO $ createFileLink actualBin (binDir </> takeFileName file')
stepToProc _ p (SymlinkLibrary file') = do
    let libDir = p </> "lib"
        actualBin = p </> file'
    liftIO $ createDirectoryIfMissing True libDir
    liftIO $ createFileLink actualBin (libDir </> takeFileName file')
stepToProc dir' _ (Write out fp) =
    liftIO (TIO.writeFile (dir' </> fp) out)
stepToProc dir' p (CopyFile src' dest') = do
    let absSrc = dir' </> src'
        absDest = p </> dest'
    putDiagnostic ("Copying file " ++ absSrc ++ " to " ++ absDest ++ "...")
    liftIO $ createDirectoryIfMissing True (takeDirectory absDest)
    liftIO $ copyFileWithMetadata absSrc absDest

processSteps :: (Traversable t)
             => FilePath -- ^ Build directory
             -> FilePath -- ^ Install directory
             -> t Command
             -> PkgM ()
processSteps pkgDir instDir = traverse_ (stepToProc pkgDir instDir)

configureInDir :: CPkg
               -> BuildVars
               -> FilePath -- ^ Build directory
               -> PkgM ()
configureInDir cpkg cfg p =

    let steps = configureCommand cpkg cfg
    in
        putNormal ("Configuring " ++ pkgName cpkg) *>
        processSteps p (installDir cfg) steps

buildInDir :: CPkg
           -> BuildVars
           -> FilePath -- ^ Build directory
           -> FilePath -- ^ Install directory
           -> PkgM ()
buildInDir cpkg cfg p p' = do
    putNormal ("Building " ++ pkgName cpkg)
    processSteps p p' (buildCommand cpkg cfg)

installInDir :: CPkg
             -> BuildVars
             -> FilePath -- ^ Build directory
             -> FilePath -- ^ Install directory
             -> PkgM ()
installInDir cpkg cfg p p' =
    putNormal ("Installing " ++ pkgName cpkg) *>
    processSteps p p' (installCommand cpkg cfg)

fetchCPkg :: CPkg
          -> FilePath -- ^ Directory for intermediate build files
          -> PkgM ()
fetchCPkg cpkg = fetchUrl (pkgUrl cpkg) (pkgName cpkg)

-- diagnosticDirectory :: String -> (FilePath -> m a) -> m a
-- diagnosticDirectory s f = f (s ++ "-diagnostic")

buildCPkg :: CPkg
          -> Maybe Platform
          -> Bool -- ^ Should we build static libraries?
          -> [FilePath] -- ^ Library directories
          -> [FilePath] -- ^ Include directories
          -> [FilePath] -- ^ Directories to add to @PATH@
          -> PkgM ()
buildCPkg cpkg host sta libs incls bins = do

    buildVars <- getVars host sta libs incls bins

    installed <- packageInstalled cpkg host buildVars

    unless installed $
        forceBuildCPkg cpkg host buildVars

getPreloads :: [ FilePath ] -> IO [ FilePath ]
getPreloads =
    fmap fold . traverse (\fp -> namesMatching (fp </> "*.so"))

-- only really suitable for hashing at this point, since we use @""@ as the
-- install directory. we use this to get a hash which we then use to get the
-- *real* install directory, which we then use with @configureVars@ to set
-- things up correctly - otherwise we would have a circularity
getVars :: Maybe Platform
        -> Bool -- ^ Should we build static libraries?
        -> [FilePath] -- ^ Library directories
        -> [FilePath] -- ^ Include directories
        -> [FilePath] -- ^ Directories to add to @PATH@
        -> PkgM BuildVars
getVars host sta links incls bins = do
    nproc <- liftIO getNumCapabilities
    lds <- liftIO $ getPreloads links
    pure (BuildVars "" host incls lds links bins dhallOS dhallArch sta nproc)

-- TODO: more complicated solver, garbage collector, and all that.
-- Basically nix-style builds for C libraries
forceBuildCPkg :: CPkg
               -> Maybe Platform
               -> BuildVars
               -> PkgM ()
forceBuildCPkg cpkg host buildVars = do

    pkgDir <- cPkgToDir cpkg host buildVars

    liftIO $ createDirectoryIfMissing True pkgDir

    withSystemTempDirectory "cpkg" $ \p -> do

        putDiagnostic ("Setting up temporary directory in " ++ p)

        fetchCPkg cpkg p

        let p' = p </> pkgSubdir cpkg

        let buildConfigured = buildVars { installDir = pkgDir }

        configureInDir cpkg buildConfigured p'

        buildInDir cpkg buildConfigured p' pkgDir

        installInDir cpkg buildConfigured p' pkgDir

        registerPkg cpkg host buildVars -- not configured? I think?
