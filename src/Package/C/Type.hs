{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Type ( CPkg (..)
                      , ConfigureVars (..)
                      , BuildVars (..)
                      , InstallVars (..)
                      , Verbosity (..)
                      , EnvVar (..)
                      , Command (..)
                      , Dep (..)
                      , Version (..)
                      , OS (..)
                      , Platform
                      -- * Helper functions
                      , cPkgDhallToCPkg
                      , showVersion
                      ) where

import           CPkgPrelude
import           Data.Hashable            (Hashable)
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import qualified Package.C.Dhall.Type     as Dhall
import           Package.C.Type.Shared
import           Package.C.Type.Vars
import           Package.C.Type.Verbosity
import           Package.C.Type.Version

data EnvVar = EnvVar { var :: String, value :: String }
            deriving (Eq, Ord, Generic, Binary, Hashable)

data Command = CreateDirectory { dir :: String }
             | MakeExecutable { file :: String }
             | Call { program     :: String
                    , arguments   :: [String]
                    , environment :: Maybe [EnvVar]
                    , procDir     :: Maybe String
                    }
             | SymlinkBinary { file :: String }
             | Write { contents :: T.Text, file :: FilePath }
             deriving (Eq, Ord, Generic, Binary, Hashable)

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
commandDhallToCommand (Dhall.SymlinkBinary b)    = SymlinkBinary (T.unpack b)
commandDhallToCommand (Dhall.Write out fp)       = Write out (T.unpack fp)

installVarsToDhallInstallVars :: InstallVars -> Dhall.InstallVars
installVarsToDhallInstallVars (InstallVars fp os) = Dhall.InstallVars (T.pack fp) os

cfgVarsToDhallCfgVars :: ConfigureVars -> Dhall.ConfigureVars
cfgVarsToDhallCfgVars (ConfigureVars dir' tgt incls lds bins os sta) = Dhall.ConfigureVars (T.pack dir') (T.pack <$> tgt) (T.pack <$> incls) (T.pack <$> lds) (T.pack <$> bins) os sta

buildVarsToDhallBuildVars :: BuildVars -> Dhall.BuildVars
buildVarsToDhallBuildVars (BuildVars nproc os tgt lds) = Dhall.BuildVars (fromIntegral nproc) os (T.pack <$> tgt) (T.pack <$> lds)

cPkgDhallToCPkg :: Dhall.CPkg -> CPkg
cPkgDhallToCPkg (Dhall.CPkg n v url subdir bldDeps deps cfgCmd buildCmd installCmd) =
    CPkg (T.unpack n) (Version v) (T.unpack url) (T.unpack subdir) bldDeps deps configure build install

    where configure cfg = commandDhallToCommand <$> cfgCmd (cfgVarsToDhallCfgVars cfg)
          build cfg = commandDhallToCommand <$> buildCmd (buildVarsToDhallBuildVars cfg)
          install cfg = commandDhallToCommand <$> installCmd (installVarsToDhallInstallVars cfg)
