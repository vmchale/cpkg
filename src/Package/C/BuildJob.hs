module Package.C.BuildJob ( buildByName
                          ) where

import           Control.Monad.IO.Class (liftIO)
import           Package.C.Build
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type
import           System.FilePath        ((</>))

-- TODO: pass link flags
buildAll :: [CPkg] -> Maybe Platform -> PkgM ()
buildAll pkgs host = buildWithContext pkgs host [] []

buildWithContext :: [CPkg]
                 -> Maybe Platform
                 -> [FilePath] -- ^ Library directories
                 -> [FilePath] -- ^ Include directories
                 -> PkgM ()
buildWithContext []   _ _ _        = pure ()
buildWithContext (c:cs) host ls is = do

    (configureVars, buildVars, installVars) <- getVars host ls is

    pkgDir <- cPkgToDir c host configureVars buildVars installVars

    let linkDir = pkgDir </> "lib"
        includeDir = pkgDir </> "include"
        links = linkDir : ls
        includes = includeDir : is

    buildCPkg c host ls is *> buildWithContext cs host links includes

buildByName :: PackId -> Maybe Platform -> PkgM ()
buildByName pkId host = do
    allPkgs <- liftIO (pkgsM pkId)
    buildAll allPkgs host
