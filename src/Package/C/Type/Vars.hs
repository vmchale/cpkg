module Package.C.Type.Vars ( BuildVars (..)
                           ) where

import           Package.C.Type.Shared

data BuildVars = BuildVars { installDir   :: FilePath
                           , targetTriple :: Maybe Platform
                           , includeDirs  :: [ FilePath ]
                           , linkDirs     :: [ FilePath ]
                           , binDirs      :: [ FilePath ]
                           , buildOS      :: OS -- ^ See [here](https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html) for terminology. This is the OS of the system we are building on.
                           , static       :: Bool
                           , cpus         :: Int
                           }
