{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Package.C.Build.Tree ( buildByName
                          ) where

import           Control.Recursion
import           CPkgPrelude
import           Data.Tree            (Tree (..))
import           Package.C.Build
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type
import           System.Directory     (doesDirectoryExist)
import           System.FilePath      ((</>))

data TreeF a x = NodeF a [x]
    deriving (Functor, Foldable, Traversable)

type instance Base (Tree a) = TreeF a

instance Recursive (Tree a) where
    project (Node x xs) = NodeF x xs

data BuildDirs = BuildDirs { libraries :: [FilePath]
                           , include   :: [FilePath]
                           , binaries  :: [FilePath]
                           }

-- FIXME: dfsForestFrom is not what we want...
buildWithContext :: Tree CPkg
                 -> Maybe Platform
                 -> Bool -- ^ Should we build static libraries?
                 -> PkgM ()
buildWithContext cTree host sta = zygoM' dirAlg buildAlg cTree

    where buildAlg :: TreeF CPkg (BuildDirs, ()) -> PkgM ()
          buildAlg (NodeF c preBds) = do

            let bds = fst <$> preBds
                go f = fold (f <$> bds)
                (ls, is, bs) = (go libraries, go include, go binaries)

            buildCPkg c host sta ls is bs

          dirAlg :: TreeF CPkg BuildDirs -> PkgM BuildDirs
          dirAlg (NodeF c bds) = do

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

            includeExists <- liftIO (doesDirectoryExist includeDir)
            let includes = if includeExists
                then includeDir : is
                else is

            pure (BuildDirs links includes bins)

buildByName :: PackId -> Maybe Platform -> Maybe String -> Bool -> PkgM ()
buildByName pkId host pkSet sta = do
    allPkgs <- liftIO (pkgsM pkId pkSet)
    buildWithContext allPkgs host sta
