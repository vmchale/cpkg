module Package.C (
                 -- * Types
                   CPkg (..)
                 , ConfigureVars (..)
                 , BuildVars (..)
                 , Version (..)
                 , Verbosity (..)
                 , PkgM
                 , Platform
                 -- * Functions
                 , buildCPkg
                 , runPkgM
                 , globalPkgDir
                 , printFlags
                 -- * Dhall functionality
                 , cPkgDhallToCPkg
                 , getCPkg
                 , getPkgs
                 ) where

import           Package.C.Build
import           Package.C.Db.Register
import           Package.C.Dhall
import           Package.C.Monad
import           Package.C.Type
