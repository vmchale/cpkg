{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Db.Type ( BuildCfg (..)
                         , InstallDb (..)
                         -- * Lenses
                         , installedPackages
                         ) where

import           Data.Binary            (Binary)
import           Data.Hashable          (Hashable)
import qualified Data.Set               as S
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Lens.Micro             (Lens')
import           Package.C.Type
import           Package.C.Type.Shared
import           Package.C.Type.Version

-- TODO: we definitely want something different here - it should allow garbage
-- collection, for one
newtype InstallDb = InstallDb { _installedPackages :: S.Set BuildCfg }
    deriving newtype (Semigroup, Monoid, Binary)

installedPackages :: Lens' InstallDb (S.Set BuildCfg)
installedPackages f s = fmap (\x -> s { _installedPackages = x }) (f (_installedPackages s))

data BuildCfg = BuildCfg { buildName       :: String
                         , buildVersion    :: Version
                         , pinnedBuildDeps :: [(T.Text, Version)]
                         , pinnedDeps      :: [(T.Text, Version)]
                         , targetArch      :: Maybe Platform
                         , configureCmds   :: [ Command ]
                         , buildCmds       :: [ Command ]
                         , installCmds     :: [ Command ]
                         -- TODO: cache build commands, configure commands, &c.
                         } deriving (Eq, Ord, Generic, Binary, Hashable)
