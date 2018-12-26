module Package.C (
                 -- * Types
                   CPkg (..)
                 , BuildVars (..)
                 , Version (..)
                 , Verbosity (..)
                 , TargetTriple (..)
                 , OS (..)
                 , Arch (..)
                 , Manufacturer (..)
                 , ABI (..)
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
                 , printCabalFlags
                 , buildByName
                 -- * Dhall functionality
                 , cPkgDhallToCPkg
                 , getCPkg
                 , getPkgs
                 -- * Packaging
                 , displayPackageSet
                 , allPackages
                 -- * Parsers
                 , parseTriple
                 , parseTripleIO
                 , parseHostIO
                 ) where

import           Package.C.Build
import           Package.C.Build.Tree
import           Package.C.Db.Register
import           Package.C.Dhall
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Triple
import           Package.C.Type
