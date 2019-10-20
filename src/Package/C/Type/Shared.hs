{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}


module Package.C.Type.Shared ( VersionBound (..)
                             , Dep (..)
                             ) where

import qualified Data.Text              as T
import           Dhall
import           Package.C.Type.Version

data VersionBound = Lower { lower :: Version }
                  | Upper { upper :: Version }
                  | LowerUpper { lower :: Version, upper :: Version }
                  | NoBound
                  deriving (Generic, FromDhall)

data Dep = Dep { name  :: T.Text
               , bound :: VersionBound
               } deriving (Generic, FromDhall)
