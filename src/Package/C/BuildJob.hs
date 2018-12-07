module Package.C.BuildJob ( buildByName
                          ) where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Containers.ListUtils
import           Package.C.Build
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type
import           System.FilePath           ((</>))

-- TODO: pass link flags
buildAll :: [CPkg] -> Maybe Platform -> PkgM ()
buildAll pkgs host = buildWithContext (nubOrdOn pkgName pkgs) host [] [] []

buildWithContext :: [CPkg]
                 -> Maybe Platform
                 -> [FilePath] -- ^ Library directories
                 -> [FilePath] -- ^ Include directories
                 -> [FilePath] -- ^ Directories to add to @PATH@
                 -> PkgM ()
buildWithContext []   _ _ _ _         = pure ()
buildWithContext (c:cs) host ls is bs = do

    (configureVars, buildVars, installVars) <- getVars host ls is bs

    pkgDir <- cPkgToDir c host configureVars buildVars installVars

    let linkDir = pkgDir </> "lib"
        linkDir64 = pkgDir </> "lib64"
        includeDir = pkgDir </> "include"
        binDir = pkgDir </> "bin"
        links = linkDir64 : linkDir : ls
        includes = includeDir : is
        bins = binDir : bs

    buildCPkg c host ls is bs *> buildWithContext cs host links includes bins

buildByName :: PackId -> Maybe Platform -> PkgM ()
buildByName pkId host = do
    allPkgs <- liftIO (pkgsM pkId)
    buildAll allPkgs host
