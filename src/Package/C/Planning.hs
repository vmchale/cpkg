{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Planning ( BuildCfg (..)
                          , InstallDb (..)
                          ) where

import           Data.Hashable          (Hashable)
import qualified Data.Set               as S
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Package.C.Type.Version

newtype InstallDb = InstallDb { installedPackages :: S.Set BuildCfg }

data BuildCfg = BuildCfg { buildName       :: String
                         , buildVersion    :: Version
                         , pinnedBuildDeps :: (T.Text, Version)
                         , pinnedDeps      :: (T.Text, Version)
                         , targetArch      :: Maybe String
                         } deriving (Generic, Hashable)


