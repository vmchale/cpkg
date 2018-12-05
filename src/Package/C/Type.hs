module Package.C.Type ( CPkg (..)
                      , ConfigureVars (..)
                      , BuildVars (..)
                      , InstallVars (..)
                      , Verbosity (..)
                      , EnvVar (..)
                      , Command (..)
                      -- * Helper functions
                      , cPkgDhallToCPkg
                      ) where

import qualified Data.Text                as T
import qualified Package.C.Dhall.Type     as Dhall
import           Package.C.Type.Shared
import           Package.C.Type.Vars
import           Package.C.Type.Verbosity
import           Package.C.Type.Version

data EnvVar = EnvVar { var :: String, value :: String }

data Command = CreateDirectory { dir :: String }
             | MakeExecutable { file :: String }
             | Call { program     :: String
                    , arguments   :: [String]
                    , environment :: Maybe [EnvVar]
                    , procDir     :: Maybe String
                    }

-- TODO: build script should take OS as an argument?
-- That way we can use make/gmake where we want it
data CPkg = CPkg { pkgName          :: String
                 , pkgVersion       :: Version
                 , pkgUrl           :: String
                 , pkgSubdir        :: String
                 , pkgBuildDeps     :: [ Dep ]
                 , pkgDeps          :: [ Dep ]
                 , configureCommand :: ConfigureVars -> [ Command ]
                 , buildCommand     :: BuildVars -> [ Command ]
                 , installCommand   :: InstallVars -> [ Command ]
                 }

envVarDhallToEnvVar :: Dhall.EnvVar -> EnvVar
envVarDhallToEnvVar (Dhall.EnvVar ev x) = EnvVar (T.unpack ev) (T.unpack x)

commandDhallToCommand :: Dhall.Command -> Command
commandDhallToCommand (Dhall.CreateDirectory d)  = CreateDirectory (T.unpack d)
commandDhallToCommand (Dhall.MakeExecutable exe) = MakeExecutable (T.unpack exe)
commandDhallToCommand (Dhall.Call p as env proc) = Call (T.unpack p) (T.unpack <$> as) (fmap envVarDhallToEnvVar <$> env) (T.unpack <$> proc)

cfgVarsToDhallCfgVars :: ConfigureVars -> Dhall.ConfigureVars
cfgVarsToDhallCfgVars (ConfigureVars dir' tgt incls os) = Dhall.ConfigureVars (T.pack dir') (T.pack <$> tgt) (T.pack <$> incls) os

buildVarsToDhallBuildVars :: BuildVars -> Dhall.BuildVars
buildVarsToDhallBuildVars (BuildVars nproc os) = Dhall.BuildVars (fromIntegral nproc) os

cPkgDhallToCPkg :: Dhall.CPkg -> CPkg
cPkgDhallToCPkg (Dhall.CPkg n v url subdir bldDeps deps cfgCmd buildCmd installCmd) =
    CPkg (T.unpack n) (Version v) (T.unpack url) (T.unpack subdir) bldDeps deps configure build install

    where configure cfg = commandDhallToCommand <$> cfgCmd (cfgVarsToDhallCfgVars cfg)
          build cfg = commandDhallToCommand <$> buildCmd (buildVarsToDhallBuildVars cfg)
          install cfg = commandDhallToCommand <$> installCmd cfg
