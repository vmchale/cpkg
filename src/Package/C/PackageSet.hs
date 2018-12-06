{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.PackageSet ( PackageSet (..)
                            , packageSetDhallToPackageSet
                            , pkgPlan
                            , pkgs
                            ) where

import           Algebra.Graph.AdjacencyMap           (edges)
import           Algebra.Graph.AdjacencyMap.Algorithm (topSort)
import           Control.Composition                  ((<=*<))
import           Data.Foldable                        (fold)
import qualified Data.Map                             as M
import qualified Data.Text                            as T
import           Dhall
import qualified Package.C.Dhall.Type                 as Dhall
import           Package.C.Type

newtype PackageSetDhall = PackageSetDhall [ Dhall.CPkg ]
    deriving Interpret

newtype PackageSet = PackageSet (M.Map T.Text CPkg)

type PackId = T.Text

-- TODO: use Algebra.Graph.AdjacencyMap.Algorithm.topSort and
-- Algebra.Graph.AdjacencyMap.Algorithm
--
-- WANT: return a @Tree [CPkg]@

packageSetDhallToPackageSet :: PackageSetDhall -> PackageSet
packageSetDhallToPackageSet (PackageSetDhall pkgs'') =
    let names = Dhall.pkgName <$> pkgs''
        pkgs' = cPkgDhallToCPkg <$> pkgs''

        in PackageSet $ M.fromList (zip names pkgs')

getDeps :: PackId -> PackageSet -> Maybe [(PackId, PackId)]
getDeps pkgName' set@(PackageSet ps) = do
    cpkg <- M.lookup pkgName' ps
    let depNames = name <$> pkgDeps cpkg
    case depNames of
        [] -> pure []
        xs -> fold <$> traverse (\p -> getDeps p set) xs

-- TODO: concurrent builds
pkgPlan :: PackId -> PackageSet -> Maybe [PackId]
pkgPlan = topSort . edges <=*< getDeps

pkgs :: PackId -> PackageSet -> Maybe [CPkg]
pkgs pkId set@(PackageSet pset) = do
    plan <- pkgPlan pkId set
    traverse (`M.lookup` pset) plan

-- next problem: pass linker flags appropriately
