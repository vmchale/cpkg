{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.PackageSet ( PackageSet (..)
                            , packageSetDhallToPackageSet
                            ) where

import qualified Data.Map             as M
import qualified Data.Text            as T
import           Dhall
import qualified Package.C.Dhall.Type as Dhall
import           Package.C.Type

newtype PackageSetDhall = PackageSetDhall [ Dhall.CPkg ]
    deriving Interpret

newtype PackageSet = PackageSet (M.Map T.Text CPkg)

packageSetDhallToPackageSet :: PackageSetDhall -> PackageSet
packageSetDhallToPackageSet (PackageSetDhall pkgs) =
    let names = Dhall.pkgName <$> pkgs
        pkgs' = cPkgDhallToCPkg <$> pkgs
        in PackageSet $ M.fromList (zip names pkgs')
