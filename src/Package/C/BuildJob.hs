module Package.C.BuildJob ( buildByName
                          ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (traverse_)
import           Package.C.Build
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type

-- TODO: pass link flags
buildAll :: [CPkg] -> Maybe Platform -> PkgM ()
buildAll pkgs host = traverse_ (\p -> buildCPkg p host [] []) pkgs

buildByName :: PackId -> Maybe Platform -> PkgM ()
buildByName pkId host = do
    allPkgs <- liftIO (pkgsM pkId)
    buildAll allPkgs host
