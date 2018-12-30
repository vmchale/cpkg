module Package.C.Type.Vars ( BuildVars (..)
                           ) where

import           Package.C.Triple.Type

data BuildVars = BuildVars { installDir   :: FilePath
                           , currentDir   :: FilePath
                           , targetTriple :: Maybe TargetTriple
                           , isCross      :: Bool
                           , includeDirs  :: [ FilePath ]
                           , preloadLibs  :: [ FilePath ]
                           , shareDirs    :: [ FilePath ]
                           , linkDirs     :: [ FilePath ]
                           , binDirs      :: [ FilePath ]
                           , buildOS      :: OS -- ^ See [here](https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html) for terminology. This is the OS of the system we are building on.
                           , buildArch    :: Arch
                           , static       :: Bool
                           , cpus         :: Int
                           }
