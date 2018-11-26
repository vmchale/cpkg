{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Dhall.Type ( CPkg (..)
                      ) where

import qualified Data.Text as T
import           Dhall

-- TODO: do we want to
data ConfigureVars = ConfigureVars { _installDir :: T.Text
                                   , _env        :: [(T.Text, T.Text)]
                                   } deriving (Generic, Inject, Interpret)

data CPkg = CPkg { _environmentVars  :: [(T.Text, T.Text)]
                 , _configureCommand :: ConfigureVars -> [ T.Text ]
                 , _buildCommand     :: [ T.Text ]
                 , _installCommand   :: [ T.Text ]
                 , _uninstallCommand :: [ T.Text ]
                 } deriving (Generic, Interpret)
