{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Type ( CPkg (..)
                      ) where

import qualified Data.Map  as M
import qualified Data.Text as T
import           Dhall

-- TODO: do we want to
data ConfigureVars = ConfigureVars { _installDir :: FilePath
                                   , _env        :: [(T.Text, T.Text)]
                                   } deriving (Generic, Interpret)

data CPkg = CPkg { _environmentVars  :: M.Map String String
                 , _configureCommand :: ConfigureVars -> [ String ]
                 , _buildCommand     :: [ String ]
                 , _installCommand   :: [ String ]
                 , _uninstallCommand :: [ String ]
                 }
