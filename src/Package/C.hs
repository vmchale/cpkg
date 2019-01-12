module Package.C (
                 -- * Types
                   CPkg (..)
                 , BuildVars (..)
                 , Version (..)
                 , Verbosity (..)
                 , TargetTriple (..)
                 , Command (..)
                 , OS (..)
                 , Arch (..)
                 , Manufacturer (..)
                 , ABI (..)
                 , InstallDb (..)
                 , BuildCfg (..)
                 , MonadDb
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
import           Package.C.Db.Monad
import           Package.C.Db.Register
import           Package.C.Db.Type
import           Package.C.Dhall
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Triple
import           Package.C.Type
