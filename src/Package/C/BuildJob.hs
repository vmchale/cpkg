{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Package.C.BuildJob ( buildByName
                          ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Recursion
import           Data.Foldable          (fold)
import           Data.Tree              (Tree (..))
import           Package.C.Build
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type
import           System.FilePath        ((</>))

-- TODO: pass link flags
buildAll :: Tree CPkg -> Maybe Platform -> Bool -> PkgM ()
buildAll = buildWithContext

data TreeF a x = NodeF a [x]
    deriving (Functor, Foldable, Traversable)

type instance Base (Tree a) = TreeF a

instance Recursive (Tree a) where
    project (Node x xs) = NodeF x xs

data BuildDirs = BuildDirs { libraries :: [FilePath]
                           , include   :: [FilePath]
                           , binaries  :: [FilePath]
                           }

buildWithContext :: Tree CPkg
                 -> Maybe Platform
                 -> Bool -- ^ Should we build static libraries?
                 -> PkgM ()
buildWithContext cTree host sta = zygoM dirAlg buildAlg cTree

    where buildAlg :: TreeF CPkg (BuildDirs, ()) -> PkgM ()
          buildAlg (NodeF c preBds) = do

            let bds = fst <$> preBds
                go f = fold (f <$> bds)
                (ls, is, bs) = (go libraries, go include, go binaries)

            buildCPkg c host sta ls is bs

          dirAlg :: TreeF CPkg BuildDirs -> PkgM BuildDirs
          dirAlg (NodeF c bds)  = do

            let go f = fold (f <$> bds)
                (ls, is, bs) = (go libraries, go include, go binaries)

            (configureVars, buildVars, installVars) <- getVars host sta ls is bs

            pkgDir <- cPkgToDir c host configureVars buildVars installVars

            let linkDir = pkgDir </> "lib"
                linkDir64 = pkgDir </> "lib64"
                includeDir = pkgDir </> "include"
                binDir = pkgDir </> "bin"
                links = linkDir64 : linkDir : ls
                bins = binDir : bs
                includes = includeDir : is

            pure (BuildDirs links includes bins)

buildByName :: PackId -> Maybe Platform -> Bool -> PkgM ()
buildByName pkId host sta = do
    allPkgs <- liftIO (pkgsM pkId)
    buildAll allPkgs host sta
