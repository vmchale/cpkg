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
                 , EnvVar (..)
                 , MonadDb
                 , PkgM
                 , Platform
                 , Dep (..)
                 -- * Functions
                 , buildCPkg
                 , runPkgM
                 , globalPkgDir
                 , printLinkerFlags
                 , printCompilerFlags
                 , printPkgConfigPath
                 , printIncludePath
                 , printLibPath
                 , printLdLibPath
                 , printCabalFlags
                 , buildByName
                 , uninstallPkgByName
                 , garbageCollect
                 , cleanCache
                 -- * Dhall functionality
                 , cPkgDhallToCPkg
                 , getCPkg
                 , getPkgs
                 -- * Packaging
                 , displayPackageSet
                 , displayPackage
                 , allPackages
                 -- * Parsers
                 , parseTriple
                 , parseTripleIO
                 , parseHostIO
                 -- * Version
                 , defaultPackageSetHash
                 ) where

import           Package.C.Build
import           Package.C.Build.Tree
import           Package.C.Db.GarbageCollect
import           Package.C.Db.Monad
import           Package.C.Db.Register
import           Package.C.Db.Type
import           Package.C.Dhall
import           Package.C.Monad
import           Package.C.PackageSet
import           Package.C.Triple
import           Package.C.Type
