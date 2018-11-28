{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , ConfigureVars (..)
                            , BuildVars (..)
                            , InstallVars (..)
                            ) where

import qualified Data.Text             as T
import           Dhall
import           GHC.Natural           (Natural)
import           Package.C.Type.Shared

data ConfigureVars = ConfigureVars { installDir   :: T.Text
                                   , targetTriple :: Maybe T.Text
                                   , includeDirs  :: [ T.Text ]
                                   , configOS     :: OS
                                   } deriving (Generic, Inject)

data BuildVars = BuildVars { cpus    :: Natural
                           , buildOS :: OS
                           }
                deriving (Generic, Inject)

data CPkg = CPkg { pkgName          :: T.Text
                 , pkgVersion       :: [ Natural ]
                 , pkgUrl           :: T.Text
                 , pkgSubdir        :: T.Text
                 , pkgBuildDeps     :: [ Dep ]
                 , pkgDeps          :: [ Dep ]
                 , configureCommand :: ConfigureVars -> [ T.Text ]
                 , executableFiles  :: [ T.Text ]
                 , buildCommand     :: BuildVars -> [ T.Text ]
                 , installCommand   :: InstallVars -> [ T.Text ]
                 } deriving (Generic, Interpret)
