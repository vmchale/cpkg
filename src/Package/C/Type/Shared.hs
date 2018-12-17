{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}


module Package.C.Type.Shared ( OS (..)
                             , VersionBound (..)
                             , Dep (..)
                             , Platform
                             ) where

import qualified Data.Text              as T
import           Dhall
import           Package.C.Type.Version

type Platform = String

data VersionBound = Lower { lower :: Version }
                  | Upper { upper :: Version }
                  | LowerUpper { lower :: Version, upper :: Version }
                  | NoBound
                  deriving (Generic, Interpret)

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
        | Haiku
        | IOS
        | AIX
        | Hurd
        | Android
        | NoOs
        deriving (Generic, Inject)
