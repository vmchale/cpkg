{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Package.C.PackageSet ( PackageSet (..)
                            , PackId
                            , pkgsM
                            , displayPackageSet
                            , displayPackage
                            , defaultPackageSetHash
                            ) where

import           CPkgPrelude
import           Data.Containers.ListUtils
import           Data.List                             (find, intersperse)
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Data.Text.Prettyprint.Doc.Render.Text
import           Dhall                                 hiding (maybe)
import qualified Package.C.Dhall.Type                  as Dhall
import           Package.C.Error
import           Package.C.Type
import           Package.C.Type.Tree

defaultPackageSetHash :: T.Text
defaultPackageSetHash = "sha256:2a0e850c768539b12542f5f29295ac6d801864d2cd7c53133cbb0b6cd89068d0"

defaultPackageSetDhall :: Maybe String -> IO PackageSetDhall
defaultPackageSetDhall (Just pkSet) = input auto (T.pack pkSet)
defaultPackageSetDhall Nothing      = input auto ("https://raw.githubusercontent.com/vmchale/cpkg/a1ccf59db55ee77b2b33e8bd8f83378ad615f636/pkgs/pkg-set.dhall " <> defaultPackageSetHash)


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
    maybe unfoundPackage pure pks
