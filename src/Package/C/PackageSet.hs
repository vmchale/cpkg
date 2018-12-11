{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Package.C.PackageSet ( PackageSet (..)
                            , PackId
                            , pkgsM
                            , displayPackageSet
                            ) where

import           Algebra.Graph.AdjacencyMap            (edges)
import           Algebra.Graph.AdjacencyMap.Algorithm  (dfsForest)
import           Data.Containers.ListUtils
import           Data.Foldable                         (fold)
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

defaultPackageSetDhall :: IO PackageSetDhall
defaultPackageSetDhall = input auto "https://raw.githubusercontent.com/vmchale/cpkg/master/pkgs/pkg-set.dhall"

displayPackageSet :: IO ()
displayPackageSet = putDoc . pretty =<< defaultPackageSetDhall

newtype PackageSetDhall = PackageSetDhall [ Dhall.CPkg ]
    deriving Interpret

instance Pretty PackageSetDhall where
    pretty (PackageSetDhall set) = vdisplay (intersperse hardline (pretty <$> set)) <> hardline

newtype PackageSet = PackageSet (M.Map T.Text CPkg)

type PackId = T.Text

data DepNames = DepNames { libNames       :: [(PackId, PackId)]
                         , buildToolNames :: [[(PackId, PackId)]]
                         }

packageSetDhallToPackageSet :: PackageSetDhall -> PackageSet
packageSetDhallToPackageSet (PackageSetDhall pkgs'') =
    let names = Dhall.pkgName <$> pkgs''
        pkgs' = cPkgDhallToCPkg <$> pkgs''

        in PackageSet $ M.fromList (zip names pkgs')

getDeps :: PackId -> PackageSet -> Maybe [(PackId, PackId)]
getDeps pkgName' set@(PackageSet ps) = do
    cpkg <- M.lookup pkgName' ps
    let depNames = (name <$> pkgDeps cpkg) ++ (name <$> pkgBuildDeps cpkg) -- this is terrible but it works better than doing nothing
    case nubOrd depNames of
        [] -> pure []
        xs -> do
            transitive <- fold <$> traverse (\p -> getDeps p set) xs
            let self = zip (repeat pkgName') xs
            pure (transitive ++ self)

unwrapForest :: [Tree a] -> Maybe (Tree a)
unwrapForest [node] = Just node
unwrapForest _      = Nothing

-- TODO: use dfsForest but check for cycles
pkgPlan :: PackId -> PackageSet -> Maybe (Tree PackId)
pkgPlan pkId ps = do
    ds <- getDeps pkId ps
    case ds of
        []  -> pure (Node pkId [])
        ds' -> unwrapForest (dfsForest (edges ds'))
        -- FIXME check for cycles with isAcyclic

pkgs :: PackId -> PackageSet -> Maybe (Tree CPkg)
pkgs pkId set@(PackageSet pset) = do
    plan <- pkgPlan pkId set
    traverse (`M.lookup` pset) plan

pkgsM :: PackId -> IO (Tree CPkg)
pkgsM pkId = do
    pks <- pkgs pkId . packageSetDhallToPackageSet <$> defaultPackageSetDhall
    case pks of
        Just x  -> pure x
        Nothing -> unfoundPackage
