module Package.C.Type.Vars ( ConfigureVars (..)
                           , BuildVars (..)
                           , InstallVars (..)
                           ) where

import           Package.C.Type.Shared

data ConfigureVars = ConfigureVars { installDir   :: FilePath
                                   , targetTriple :: Maybe Platform
                                   , includeDirs  :: [ FilePath ]
                                   , linkDirs     :: [ FilePath ]
                                   , binDirs      :: [ FilePath ]
                                   , configOS     :: OS
                                   , static       :: Bool
                                   }

data BuildVars = BuildVars { cpus           :: Int
                           , buildOS        :: OS
                           , buildTgt       :: Maybe Platform
                           , linkDirsBld    :: [ FilePath ]
                           , includeDirsBld :: [ FilePath ]
                           }

data InstallVars = InstallVars { installPath :: FilePath
                               , installOS   :: OS
                               }
