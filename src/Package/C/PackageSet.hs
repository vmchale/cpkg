{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Package.C.PackageSet ( PackageSet (..)
                            , PackId
                            , pkgsM
                            , displayPackageSet
                            , displayPackage
                            ) where

import           CPkgPrelude
import           Data.Containers.ListUtils
import           Data.List                             (find, intersperse)
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Data.Text.Prettyprint.Doc.Render.Text
import           Dhall
import qualified Package.C.Dhall.Type                  as Dhall
import           Package.C.Error
import           Package.C.Type
import           Package.C.Type.Tree

defaultPackageSetDhall :: Maybe String -> IO PackageSetDhall
defaultPackageSetDhall (Just pkSet) = input auto (T.pack pkSet)
defaultPackageSetDhall Nothing      = input auto "https://raw.githubusercontent.com/vmchale/cpkg/7d3e3aaac89c807675ec4cfc0e34af750b40f883/pkgs/pkg-set.dhall sha256:cc6786ef0853da1527487cbb1cf9ea62c2918ddf218e9abc16839a90eb50d94d"

displayPackageSet :: Maybe String -> IO ()
displayPackageSet = putDoc . pretty <=< defaultPackageSetDhall

displayPackage :: String -> IO ()
displayPackage str = do
    pk <- find (\ps -> T.unpack (Dhall.pkgName ps) == str) . listPackages <$> defaultPackageSetDhall Nothing
    case pk of
        Just p  -> putDoc (pretty p <> hardline)
        Nothing -> unfoundPackage

newtype PackageSetDhall = PackageSetDhall { listPackages :: [ Dhall.CPkg ] }
    deriving FromDhall

instance Pretty PackageSetDhall where
    pretty (PackageSetDhall set) = vdisplay (intersperse hardline (pretty <$> set)) <> hardline

newtype PackageSet = PackageSet (M.Map T.Text CPkg)

type PackId = T.Text

packageSetDhallToPackageSet :: PackageSetDhall -> PackageSet
packageSetDhallToPackageSet (PackageSetDhall pkgs'') =
    let names = Dhall.pkgName <$> pkgs''
        pkgs' = cPkgDhallToCPkg <$> pkgs''

        in PackageSet $ M.fromList (zip names pkgs')

getDeps :: PackId -> Bool -> PackageSet -> Maybe (DepTree PackId)
getDeps pkgName' usr set@(PackageSet ps) = do
    cpkg <- M.lookup pkgName' ps
    let depNames = name <$> pkgDeps cpkg
        bldDepNames = name <$> pkgBuildDeps cpkg
        ds = nubOrd depNames
        bds = nubOrd bldDepNames
    nextDeps <- traverse (\p -> getDeps p False set) ds
    nextBldDeps <- traverse (\p -> asBldDep <$> getDeps p False set) bds
    pure $ DepNode pkgName' usr (nextDeps ++ nextBldDeps)

-- TODO: use dfsForest but check for cycles
pkgPlan :: PackId -> PackageSet -> Maybe (DepTree PackId)
pkgPlan pkId = getDeps pkId True -- manually installed

pkgs :: PackId -> PackageSet -> Maybe (DepTree CPkg)
pkgs pkId set@(PackageSet pset) = do
    plan <- pkgPlan pkId set
    traverse (`M.lookup` pset) plan

pkgsM :: PackId -> Maybe String -> IO (DepTree CPkg)
pkgsM pkId pkSet = do
    pks <- pkgs pkId . packageSetDhallToPackageSet <$> defaultPackageSetDhall pkSet
    case pks of
        Just x  -> pure x
        Nothing -> unfoundPackage
