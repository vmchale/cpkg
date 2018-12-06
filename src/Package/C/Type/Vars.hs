module Package.C.Type.Vars ( ConfigureVars (..)
                           , BuildVars (..)
                           ) where

import           Package.C.Type.Shared

data ConfigureVars = ConfigureVars { installDir   :: FilePath
                                   , targetTriple :: Maybe Platform
                                   , includeDirs  :: [ FilePath ]
                                   , linkDirs     :: [ FilePath ]
                                   , configOS     :: OS
                                   }

data BuildVars = BuildVars { cpus    :: Int
                           , osBuild :: OS
                           }
