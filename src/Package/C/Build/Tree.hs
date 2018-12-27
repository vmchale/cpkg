module Package.C.Build.Tree ( buildByName
                            ) where

import           Control.Recursion
import           CPkgPrelude
import           Package.C.Build
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type
import           Package.C.Type.Tree
import           System.Directory     (doesDirectoryExist)
import           System.FilePath      ((</>))

data BuildDirs = BuildDirs { libraries :: [FilePath]
                           , share     :: [FilePath]
                           , include   :: [FilePath]
                           , binaries  :: [FilePath]
                           }

getAll :: [BuildDirs] -> BuildDirs
getAll bds =
    let go f = fold (f <$> bds)
    in BuildDirs (go libraries) (go share) (go include) (go binaries)

buildWithContext :: DepTree CPkg
                 -> Maybe TargetTriple
                 -> Bool -- ^ Should we build static libraries?
                 -> PkgM ()
buildWithContext cTree host sta = zygoM' dirAlg buildAlg cTree

    where buildAlg :: DepTreeF CPkg (BuildDirs, ()) -> PkgM ()
          buildAlg (DepNodeF c preBds) =
            buildCPkg c host sta ds ls is bs
                where (BuildDirs ls ds is bs) = getAll (fst <$> preBds)
          buildAlg (BldDepNodeF c preBds) =
            buildCPkg c Nothing False ds ls is bs -- don't use static libraries for build dependencies
                where (BuildDirs ls ds is bs) = getAll (fst <$> preBds)

          dirAlg :: DepTreeF CPkg BuildDirs -> PkgM BuildDirs
          dirAlg dep = do

            let (BuildDirs ls ds is bs) = getAll $ deps dep

            buildVars <- getVars host sta ds ls is bs

            pkgDir <- cPkgToDir (self dep) host buildVars

            let linkDir = pkgDir </> "lib"
                linkDir64 = pkgDir </> "lib64"
                includeDir = pkgDir </> "include"
                dataDir = pkgDir </> "share"
                binDir = pkgDir </> "bin"
                links = linkDir64 : linkDir : ls
                bins = binDir : bs
                shares = dataDir : ds

            includeExists <- liftIO (doesDirectoryExist includeDir)
            let includes = if includeExists
                then includeDir : is
                else is

            pure (BuildDirs links shares includes bins)

-- TODO: should this parse a string into a TargetTriple instead?
buildByName :: PackId -> Maybe TargetTriple -> Maybe String -> Bool -> PkgM ()
buildByName pkId host pkSet sta = do
    allPkgs <- liftIO (pkgsM pkId pkSet)
    buildWithContext allPkgs host sta
