module Package.C.BuildJob ( buildAll
                          ) where

import           Data.Foldable   (traverse_)
import           Package.C.Build
import           Package.C.Monad
import           Package.C.Type

-- TODO: pass link flags
buildAll :: [CPkg] -> Maybe Platform -> PkgM ()
buildAll pkgs host = traverse_ (flip buildCPkg host) pkgs
