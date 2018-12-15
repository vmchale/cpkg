{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Package.C.PackageSet ( PackageSet (..)
                            , PackId
                            , pkgsM
                            , displayPackageSet
                            ) where

import           CPkgPrelude
import           Data.Containers.ListUtils
import           Data.List                             (intersperse)
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Tree                             (Tree (..))
import           Dhall
import qualified Package.C.Dhall.Type                  as Dhall
import           Package.C.Error
import           Package.C.Type

defaultPackageSetDhall :: Maybe String -> IO PackageSetDhall
defaultPackageSetDhall (Just pkSet) = input auto (T.pack pkSet)
defaultPackageSetDhall Nothing      = input auto "https://raw.githubusercontent.com/vmchale/cpkg/master/pkgs/pkg-set.dhall"

displayPackageSet :: Maybe String -> IO ()
displayPackageSet = putDoc . pretty <=< defaultPackageSetDhall

newtype PackageSetDhall = PackageSetDhall [ Dhall.CPkg ]
    deriving Interpret

instance Pretty PackageSetDhall where
    pretty (PackageSetDhall set) = vdisplay (intersperse hardline (pretty <$> set)) <> hardline

newtype PackageSet = PackageSet (M.Map T.Text CPkg)

type PackId = T.Text

packageSetDhallToPackageSet :: PackageSetDhall -> PackageSet
packageSetDhallToPackageSet (PackageSetDhall pkgs'') =
    let names = Dhall.pkgName <$> pkgs''
        pkgs' = cPkgDhallToCPkg <$> pkgs''

        in PackageSet $ M.fromList (zip names pkgs')

-- FIXME: this whole thing is bad
getDeps :: PackId -> PackageSet -> Maybe (Tree PackId)
getDeps pkgName' set@(PackageSet ps) = do
    cpkg <- M.lookup pkgName' ps
    let depNames = (name <$> pkgDeps cpkg) ++ (name <$> pkgBuildDeps cpkg)
    case nubOrd depNames of
        xs -> Node pkgName' <$> traverse (\p -> getDeps p set) xs

-- TODO: use dfsForest but check for cycles
pkgPlan :: PackId -> PackageSet -> Maybe (Tree PackId)
pkgPlan = getDeps

pkgs :: PackId -> PackageSet -> Maybe (Tree CPkg)
pkgs pkId set@(PackageSet pset) = do
    plan <- pkgPlan pkId set
    traverse (`M.lookup` pset) plan

pkgsM :: PackId -> Maybe String -> IO (Tree CPkg)
pkgsM pkId pkSet = do
    pks <- pkgs pkId . packageSetDhallToPackageSet <$> defaultPackageSetDhall pkSet
    case pks of
        Just x  -> pure x
        Nothing -> unfoundPackage
