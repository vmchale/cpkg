module Package.C.Type ( CPkg (..)
                      , ConfigureVars (..)
                      , BuildVars (..)
                      , InstallVars (..)
                      , Verbosity (..)
                      -- * Helper functions
                      , cPkgDhallToCPkg
                      ) where

import qualified Data.Text              as T
import qualified Package.C.Dhall.Type   as Dhall
import           Package.C.Type.Shared
import           Package.C.Type.Version

data Verbosity = Silent -- ^ Display nothing
               | Normal -- ^ Display progress information
               | Verbose -- ^ Display stderr from builds
               | Loud -- ^ Display stdout and stderr from builds
               | Diagnostic -- ^ Display stdout and stderr from builds, and display debug information
               deriving (Eq, Ord)

-- TODO: hashable?
data ConfigureVars = ConfigureVars { installDir   :: FilePath
                                   , targetTriple :: Maybe String
                                   , includeDirs  :: [ FilePath ]
                                   , configOS     :: OS
                                   }

data BuildVars = BuildVars { cpus    :: Int
                           , osBuild :: OS
                           }

-- TODO: build script should take OS as an argument?
-- That way we can use make/gmake where we want it
data CPkg = CPkg { pkgName          :: String
                 , pkgVersion       :: Version
                 , pkgUrl           :: String
                 , pkgSubdir        :: String
                 , pkgBuildDeps     :: [ Dhall.Dep ]
                 , pkgDeps          :: [ Dhall.Dep ]
                 , configureCommand :: ConfigureVars -> [ String ]
                 , executableFiles  :: [ String ]
                 , buildCommand     :: BuildVars -> [ String ]
                 , installCommand   :: InstallVars -> [ String ]
                 }

cfgVarsToDhallCfgVars :: ConfigureVars -> Dhall.ConfigureVars
cfgVarsToDhallCfgVars (ConfigureVars dir tgt incls os) = Dhall.ConfigureVars (T.pack dir) (T.pack <$> tgt) (T.pack <$> incls) os

buildVarsToDhallBuildVars :: BuildVars -> Dhall.BuildVars
buildVarsToDhallBuildVars (BuildVars nproc os) = Dhall.BuildVars (fromIntegral nproc) os

cPkgDhallToCPkg :: Dhall.CPkg -> CPkg
cPkgDhallToCPkg (Dhall.CPkg name v url subdir bldDeps deps cfgCmd exes buildCmd installCmd) =
    CPkg (T.unpack name) (Version v) (T.unpack url) (T.unpack subdir) bldDeps deps configure (T.unpack <$> exes) build install

    where configure cfg = T.unpack <$> cfgCmd (cfgVarsToDhallCfgVars cfg)
          build cfg = T.unpack <$> buildCmd (buildVarsToDhallBuildVars cfg)
          install cfg = T.unpack <$> installCmd cfg
