{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , ConfigureVars (..)
                            ) where

import qualified Data.Text as T
import           Dhall

-- TODO: do we want to
newtype ConfigureVars = ConfigureVars { _installDir :: T.Text
                                      } deriving newtype (Inject)

data CPkg = CPkg { _pkgName          :: T.Text
                 , _pkgUrl           :: T.Text
                 , _configureCommand :: ConfigureVars -> [ T.Text ]
                 , _buildCommand     :: [ T.Text ] -- TODO: should take number of cores as an argument
                 , _installCommand   :: [ T.Text ]
                 } deriving (Generic, Interpret)
