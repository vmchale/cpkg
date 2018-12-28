module Package.C.Build.Tree ( buildByName
                            ) where

import           Control.Recursion
import           CPkgPrelude
import           Data.List            (isInfixOf)
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

-- in order to prevent the "vanilla" libffi from preceding the *cross* libffi,
-- we filter out any directory that doesn't contain the target triple. this
-- causes further bugs and it's slow
immoralFilter :: Maybe TargetTriple -> [FilePath] -> [FilePath]
immoralFilter Nothing fps = fps
immoralFilter (Just tgt') fps =
    let infixDir = show tgt'
    in filter (\fp -> infixDir `isInfixOf` fp || "meson" `isInfixOf` fp) fps

buildWithContext :: DepTree CPkg
                 -> Maybe TargetTriple
                 -> Bool -- ^ Should we build static libraries?
                 -> PkgM ()
buildWithContext cTree host sta = zygoM' dirAlg buildAlg cTree

    where buildAlg :: DepTreeF CPkg (BuildDirs, ()) -> PkgM ()
          buildAlg (DepNodeF c preBds) =
            buildCPkg c host sta ds (immoralFilter host ls) is bs
                where (BuildDirs ls ds is bs) = getAll (fst <$> preBds)
          buildAlg (BldDepNodeF c preBds) =
            buildCPkg c Nothing False ds ls is bs -- don't use static libraries for build dependencies
                where (BuildDirs ls ds is bs) = getAll (fst <$> preBds)

          mkBuildDirs :: MonadIO m => FilePath -> BuildDirs -> m BuildDirs
          mkBuildDirs pkgDir (BuildDirs ls ds is bs) = do
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

          dirAlg :: DepTreeF CPkg BuildDirs -> PkgM BuildDirs
          dirAlg (DepNodeF c bds) = do

            let bldDirs@(BuildDirs ls ds is bs) = getAll bds

            buildVars <- getVars host sta ds ls is bs

            pkgDir <- cPkgToDir c host buildVars

            mkBuildDirs pkgDir bldDirs

          dirAlg (BldDepNodeF c bds) = do

            let bldDirs@(BuildDirs ls ds is bs) = getAll bds

            buildVars <- getVars Nothing False ds ls is bs

            pkgDir <- cPkgToDir c Nothing buildVars

            mkBuildDirs pkgDir bldDirs

-- TODO: should this parse a string into a TargetTriple instead?
buildByName :: PackId -> Maybe TargetTriple -> Maybe String -> Bool -> PkgM ()
buildByName pkId host pkSet sta = do
    allPkgs <- liftIO (pkgsM pkId pkSet)
    buildWithContext allPkgs host sta
