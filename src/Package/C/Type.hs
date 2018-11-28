module Package.C.Type ( CPkg (..)
                      , ConfigureVars (..)
                      , Verbosity (..)
                      -- * Helper functions
                      , cPkgDhallToCPkg
                      ) where

import qualified Data.Text            as T
import qualified Package.C.Dhall.Type as Dhall
import           Package.C.Version

data Verbosity = Silent -- ^ Display nothing
               | Normal -- ^ Display progress information
               | Verbose -- ^ Display stderr from builds
               | Loud -- ^ Display stdout and stderr from builds
               | Diagnostic -- ^ Display stdout and stderr from builds, and display debug information
               deriving (Eq, Ord)

data ConfigureVars = ConfigureVars { _installDir  :: FilePath
                                   , _includeDirs :: [ FilePath ]
                                   }

data CPkg = CPkg { _pkgName          :: String
                 , _pkgVersion       :: Version
                 , _pkgUrl           :: String
                 , _pkgSubdir        :: String
                 , _configureCommand :: ConfigureVars -> [ String ]
                 , _executableFiles  :: [ String ]
                 , _buildCommand     :: [ String ]
                 , _installCommand   :: [ String ]
                 }

cfgVarsToDhallCfgVars :: ConfigureVars -> Dhall.ConfigureVars
cfgVarsToDhallCfgVars (ConfigureVars dir incls) = Dhall.ConfigureVars (T.pack dir) (T.pack <$> incls)

cPkgDhallToCPkg :: Dhall.CPkg -> CPkg
cPkgDhallToCPkg (Dhall.CPkg name v url subdir cfgCmd exes buildCmd installCmd) =
    CPkg (T.unpack name) (Version (fromIntegral <$> v)) (T.unpack url) (T.unpack subdir) (\cfg -> T.unpack <$> cfgCmd (cfgVarsToDhallCfgVars cfg)) (T.unpack <$> exes) (T.unpack <$> buildCmd) (T.unpack <$> installCmd)
