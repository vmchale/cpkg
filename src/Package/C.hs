module Package.C (
                 -- * Types
                   CPkg (..)
                 , BuildVars (..)
                 , Version (..)
                 , Verbosity (..)
                 , PkgM
                 , Platform
                 -- * Functions
                 , buildCPkg
                 , runPkgM
                 , globalPkgDir
                 , printLinkerFlags
                 , printCompilerFlags
                 , printPkgConfigPath
                 , printIncludePath
                 , printLibPath
                 , buildByName
                 -- * Dhall functionality
                 , cPkgDhallToCPkg
                 , getCPkg
                 , getPkgs
                 -- * Packaging
                 , displayPackageSet
                 , allPackages
                 ) where

import           Package.C.Build
import           Package.C.Build.Tree
import           Package.C.Db.Register
import           Package.C.Dhall
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Type
