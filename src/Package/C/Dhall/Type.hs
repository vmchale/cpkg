{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , ConfigureVars (..)
                            ) where

import qualified Data.Text as T
import           Dhall

-- TODO: do we want to
data ConfigureVars = ConfigureVars { _installDir  :: T.Text
                                   , _includeDirs :: [ T.Text ]
                                   } deriving (Generic, Inject)

data CPkg = CPkg { _pkgName          :: T.Text
                 , _pkgUrl           :: T.Text
                 , _configureCommand :: ConfigureVars -> [ T.Text ]
                 , _buildCommand     :: [ T.Text ] -- TODO: should take number of cores as an argument
                 , _installCommand   :: [ T.Text ]
                 } deriving (Generic, Interpret)
