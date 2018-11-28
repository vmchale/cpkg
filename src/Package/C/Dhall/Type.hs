{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , ConfigureVars (..)
                            , BuildVars (..)
                            ) where

import qualified Data.Text   as T
import           Dhall
import           GHC.Natural (Natural)

data ConfigureVars = ConfigureVars { _installDir  :: T.Text
                                   , _includeDirs :: [ T.Text ]
                                   } deriving (Generic, Inject)

newtype BuildVars = BuildVars { _cpus :: Natural }
                  deriving newtype Inject

data CPkg = CPkg { _pkgName          :: T.Text
                 , _pkgVersion       :: [ Natural ]
                 , _pkgUrl           :: T.Text
                 , _pkgSubdir        :: T.Text
                 , _configureCommand :: ConfigureVars -> [ T.Text ]
                 , _executableFiles  :: [ T.Text ]
                 , _buildCommand     :: BuildVars -> [ T.Text ]
                 , _installCommand   :: [ T.Text ]
                 } deriving (Generic, Interpret)
