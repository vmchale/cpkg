{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , ConfigureVars (..)
                            ) where

import qualified Data.Text as T
import           Dhall

data ConfigureVars = ConfigureVars { _installDir  :: T.Text
                                   , _includeDirs :: [ T.Text ]
                                   } deriving (Generic, Inject)

data CPkg = CPkg { _pkgName          :: T.Text
                 , _pkgUrl           :: T.Text
                 , _pkgSubdir        :: T.Text
                 , _configureCommand :: ConfigureVars -> [ T.Text ]
                 , _executableFiles  :: [ T.Text ]
                 , _buildCommand     :: [ T.Text ]
                 , _installCommand   :: [ T.Text ]
                 } deriving (Generic, Interpret)
