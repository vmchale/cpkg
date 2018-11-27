module Package.C.Type ( CPkg (..)
                      , ConfigureVars (..)
                      ) where

-- import qualified Package.C.Dhall.Type as Dhall

newtype ConfigureVars = ConfigureVars { _installDir :: String
                                      }

data CPkg = CPkg { _pkgUrl           :: String
                 , _configureCommand :: ConfigureVars -> [ String ]
                 , _buildCommand     :: [ String ]
                 , _installCommand   :: [ String ]
                 }
