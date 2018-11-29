{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Type.Shared ( OS (..)
                             , VersionBound (..)
                             , InstallVars (..)
                             , Dep (..)
                             ) where

import qualified Data.Text              as T
import           Dhall
import           Package.C.Type.Version

data VersionBound = Lower { lower :: Version }
                  | Upper { upper :: Version }
                  | LowerUpper { lower :: Version, upper :: Version }
                  | NoBound
                  deriving (Generic, Interpret)

newtype InstallVars = InstallVars { installOS :: OS }
                    deriving newtype Inject

data Dep = Dep { name  :: T.Text
               , bound :: VersionBound
               } deriving (Generic, Interpret)

data OS = Darwin
        | Dragonfly
        | FreeBSD
        | Linux
        | OpenBSD
        | NetBSD
        | Solaris
        | Windows
        | Redox
        | NoOs
        deriving (Generic, Inject)
