module Package.C.Type.Vars ( BuildVars (..)
                           ) where

import           Package.C.Type.Shared

data BuildVars = BuildVars { installDir   :: FilePath
                           , targetTriple :: Maybe Platform
                           , includeDirs  :: [ FilePath ]
                           , linkDirs     :: [ FilePath ]
                           , binDirs      :: [ FilePath ]
                           , configOS     :: OS
                           , static       :: Bool
                           , cpus         :: Int
                           }
