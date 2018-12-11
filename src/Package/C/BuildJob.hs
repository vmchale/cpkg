module Package.C.BuildJob ( buildByName
                          ) where

import           Control.Monad.IO.Class (liftIO)
import           Package.C.Build
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type
import           System.Directory       (doesDirectoryExist)
import           System.FilePath        ((</>))

-- TODO: pass link flags
buildAll :: [CPkg] -> Maybe Platform -> Bool -> PkgM ()
buildAll pkgs host sta = buildWithContext pkgs host sta [] [] []

buildWithContext :: [CPkg]
                 -> Maybe Platform
                 -> Bool -- ^ Should we build static libraries?
                 -> [FilePath] -- ^ Library directories
                 -> [FilePath] -- ^ Include directories
                 -> [FilePath] -- ^ Directories to add to @PATH@
                 -> PkgM ()
buildWithContext [] _ _ _ _ _         = pure ()
buildWithContext (c:cs) host sta ls is bs = do

    (configureVars, buildVars, installVars) <- getVars host sta ls is bs

    pkgDir <- cPkgToDir c host configureVars buildVars installVars

    let linkDir = pkgDir </> "lib"
        linkDir64 = pkgDir </> "lib64"
        includeDir = pkgDir </> "include"
        binDir = pkgDir </> "bin"
        links = linkDir64 : linkDir : ls
        bins = binDir : bs

    buildCPkg c host sta ls is bs

    includeExists <- liftIO (doesDirectoryExist includeDir)
    let includes = if includeExists
        then includeDir : is
        else is

    buildWithContext cs host sta links includes bins

buildByName :: PackId -> Maybe Platform -> Bool -> PkgM ()
buildByName pkId host sta = do
    allPkgs <- liftIO (pkgsM pkId)
    buildAll allPkgs host sta
