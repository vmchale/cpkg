{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Dhall.Type ( CPkg (..)
                      ) where

import qualified Data.Text as T
import           Dhall

-- TODO: do we want to
newtype ConfigureVars = ConfigureVars { _installDir :: T.Text
                                      } deriving newtype (Inject)

data CPkg = CPkg { _pkgUrl           :: T.Text
                 , _configureCommand :: ConfigureVars -> [ T.Text ]
                 , _buildCommand     :: [ T.Text ]
                 , _installCommand   :: [ T.Text ]
                 } deriving (Generic, Interpret)
