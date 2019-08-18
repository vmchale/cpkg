module Package.C.Build ( buildCPkg
                       , getVars
                       , cPkgToDir
                       ) where

import           Control.Concurrent          (getNumCapabilities)
import           CPkgPrelude
import           Data.List                   (isInfixOf)
import           Data.Maybe                  (isJust)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Package.C.Build.OS
import           Package.C.Db.Register
import           Package.C.Fetch
import           Package.C.Logging
import           Package.C.Monad
import           Package.C.Type
import           System.Directory
import           System.Directory.Executable (mkExecutable)
import           System.Directory.Recursive  (getSubdirsRecursive)
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
stepToProc _ p (SymlinkManpage file' sec) = do
    manDir <- (</> ("share" </> "man" </> "man" ++ show sec)) <$> globalPkgDir
    let actualMan = p </> file'
    liftIO $ createDirectoryIfMissing True manDir
    liftIO $ createFileLink actualMan (manDir </> takeFileName file')
stepToProc _ p (Symlink tgt' lnk) = do
    let linkAbs = p </> lnk
    putDiagnostic ("Creating directory" ++ takeDirectory linkAbs ++ "...")
    liftIO $ createDirectoryIfMissing True (takeDirectory linkAbs)
    -- TODO: diagnostics for symlinks
    liftIO $ createFileLink (p </> tgt') linkAbs
stepToProc dir' _ (Write out fp) = do
    let fpAbs = dir' </> fp
    putDiagnostic ("Writing\n" ++ T.unpack out ++ "\n in file" ++ fpAbs)
    liftIO (TIO.writeFile fpAbs out)
stepToProc dir' p (CopyFile src' dest') = do
    let absSrc = dir' </> src'
        absDest = p </> dest'
    putDiagnostic ("Copying file " ++ absSrc ++ " to " ++ absDest ++ "...")
    liftIO $ createDirectoryIfMissing True (takeDirectory absDest)
    liftIO $ copyFileWithMetadata absSrc absDest
stepToProc dir' _ (Patch contents') = do
    liftIO $ TIO.writeFile (dir' </> "step.patch") contents'
    waitProcess $ (proc "patch" ["-p0", "-i", "step.patch"]) { cwd = Just dir' }

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
fetchCPkg cpkg = fetchUrl (pkgUrl cpkg) (pkgName cpkg) (pkgStream cpkg)

buildCPkg :: CPkg
          -> Maybe TargetTriple
          -> Bool -- ^ Should we build static libraries?
          -> Bool -- ^ Should we install globally?
          -> Bool -- ^ Was this package installed manually?
          -> [FilePath] -- ^ Shared data directories
          -> [FilePath] -- ^ Library directories
          -> [FilePath] -- ^ Include directories
          -> [FilePath] -- ^ Directories to add to @PATH@
          -> PkgM ()
buildCPkg cpkg host sta glob usr shr libs incls bins = do

    buildVars <- getVars host sta shr libs incls bins

    -- TODO: use a real database
    installed <- packageInstalled cpkg host glob buildVars

    when installed $
        putDiagnostic ("Package " ++ pkgName cpkg ++ " already installed, skipping.")

    unless installed $
        forceBuildCPkg cpkg host glob usr buildVars

getPreloads :: [ FilePath ] -> IO [ FilePath ]
getPreloads =
    fmap fold . traverse (\fp -> namesMatching (fp </> "*.so"))

-- only really suitable for hashing at this point, since we use @""@ as the
-- install directory. we use this to get a hash which we then use to get the
-- *real* install directory, which we then use with @configureVars@ to set
-- things up correctly - otherwise we would have a circularity
getVars :: Maybe TargetTriple
        -> Bool -- ^ Should we build static libraries?
        -> [FilePath] -- ^ Shared data directories
        -> [FilePath] -- ^ Library directories
        -> [FilePath] -- ^ Include directories
        -> [FilePath] -- ^ Directories to add to @PATH@
        -> PkgM BuildVars
getVars host sta shr links incls bins = do
    nproc <- liftIO getNumCapabilities
    pure (BuildVars "" "" host (isJust host) incls [] shr links bins dhallOS dhallArch sta nproc)
    -- we don't run getPreloads until later because that might be slow

-- diagnosticDirectory :: String -> (FilePath -> m a) -> m a
-- diagnosticDirectory s f = f (s ++ "-diagnostic")

getSubdirsWrap :: FilePath -> IO [FilePath]
getSubdirsWrap fp = do
    b <- doesDirectoryExist fp
    if b
        then (fp:) <$> getSubdirsRecursive fp
        else pure []

-- TODO: more complicated solver, garbage collector, and all that.
-- Basically nix-style builds for C libraries
forceBuildCPkg :: CPkg
               -> Maybe TargetTriple
               -> Bool
               -> Bool -- ^ Manually installed?
               -> BuildVars
               -> PkgM ()
forceBuildCPkg cpkg host glob usr buildVars = do

    pkgDir <- cPkgToDir cpkg host glob buildVars

    liftIO $ createDirectoryIfMissing True pkgDir

    withSystemTempDirectory "cpkg" $ \p -> do
    -- diagnosticDirectory "cpkg" $ \p -> do

        putDiagnostic ("Setting up temporary directory in " ++ p)

        fetchCPkg cpkg p

        pAbs <- liftIO (makeAbsolute p)

        let p' = pAbs </> pkgSubdir cpkg

        lds <- liftIO $ do
            linkSubdirs <- concat <$> traverse getSubdirsWrap (linkDirs buildVars)
            -- FIXME: this seems stupid
            let curses = not . ("curses" `isInfixOf`)
            getPreloads $ filter curses linkSubdirs

        let buildConfigured = buildVars { installDir = pkgDir, currentDir = pAbs, preloadLibs = lds }

        configureInDir cpkg buildConfigured p'

        buildInDir cpkg buildConfigured p' pkgDir

        installInDir cpkg buildConfigured p' pkgDir

        registerPkg cpkg host glob usr buildVars -- not configured
