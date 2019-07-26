{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Db.Type ( BuildCfg (..)
                         , InstallDb (..)
                         -- * Lenses
                         , installedPackages
                         ) where

import           CPkgPrelude
import           Data.Semigroup
import qualified Data.Set       as S
import qualified Data.Text      as T
import           Package.C.Type

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
                         , targetArch      :: Maybe TargetTriple
                         , global          :: Bool
                         , configureCmds   :: [ Command ]
                         , buildCmds       :: [ Command ]
                         , installCmds     :: [ Command ]
                         -- , tarball         :: FilePath
                         , manual          :: Bool -- ^ Was this package manually installed?
                         } deriving (Eq, Ord, Generic, Binary, Hashable)
