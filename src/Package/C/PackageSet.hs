{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Package.C.PackageSet ( PackageSet (..)
                            , PackId
                            , pkgsM
                            , displayPackageSet
                            ) where

import           Algebra.Graph.AdjacencyMap            (edges)
import           Algebra.Graph.AdjacencyMap.Algorithm  (topSort)
import           Control.Composition                   ((<=*<))
import           Data.Foldable                         (fold)
import           Data.List                             (intersperse)
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Data.Text.Prettyprint.Doc.Render.Text
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
        xs -> do
            transitive <- fold <$> traverse (\p -> getDeps p set) xs
            let self = zip (repeat pkgName') xs
            pure (transitive ++ self)

-- TODO: concurrent builds
pkgPlan :: PackId -> PackageSet -> Maybe [PackId]
pkgPlan pkId set = (++ [pkId]) <$> plan pkId set
    where plan = fmap reverse . topSort . edges <=*< getDeps

pkgs :: PackId -> PackageSet -> Maybe [CPkg]
pkgs pkId set@(PackageSet pset) = do
    plan <- pkgPlan pkId set
    traverse (`M.lookup` pset) plan

pkgsM :: PackId -> IO [CPkg]
pkgsM pkId = do
    pks <- pkgs pkId . packageSetDhallToPackageSet <$> defaultPackageSetDhall
    case pks of
        Just x  -> pure x
        Nothing -> unfoundPackage
