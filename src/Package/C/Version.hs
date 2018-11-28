{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Version ( Version (..)
                         , VersionBound (..)
                         , showVersion
                         ) where

import           Data.List (intercalate)
import           Dhall

newtype Version = Version [ Natural ]
    deriving newtype Interpret

data VersionBound = Lower { lower :: Version }
                  | Upper { upper :: Version }
                  | LowerUpper { lower :: Version, upper :: Version }
                  | NoBound
                  deriving (Generic, Interpret)

showVersion :: Version -> String
showVersion (Version v) = intercalate "." (show <$> v)
