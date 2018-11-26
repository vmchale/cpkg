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
                                      } deriving newtype (Inject, Interpret)

data CPkg = CPkg { _environmentVars  :: [(T.Text, T.Text)]
                 , _configureCommand :: ConfigureVars -> [ T.Text ]
                 , _buildCommand     :: [ T.Text ]
                 , _installCommand   :: [ T.Text ]
                 , _uninstallCommand :: [ T.Text ]
                 } deriving (Generic, Interpret)
