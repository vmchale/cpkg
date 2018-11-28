{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Type.Shared ( OS (..)
                             , VersionBound (..)
                             , InstallVars (..)
                             ) where

import           Dhall
import           Package.C.Type.Version

data VersionBound = Lower { lower :: Version }
                  | Upper { upper :: Version }
                  | LowerUpper { lower :: Version, upper :: Version }
                  | NoBound
                  deriving (Generic, Interpret)

newtype InstallVars = InstallVars { installOS :: OS }
                    deriving newtype Inject

data OS = Darwin
        | Dragonfly
        | FreeBSD
        | Linux
        | OpenBSD
        | NetBSD
        | Solaris
        | Windows
        deriving (Generic, Inject, Interpret)
