{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Type.Version ( Version (..)
                         , showVersion
                         ) where

import           Data.Binary   (Binary)
import           Data.Hashable (Hashable)
import           Data.List     (intercalate)
import           Dhall

newtype Version = Version [ Natural ]
    deriving (Eq, Ord, Interpret, Binary, Hashable)

showVersion :: Version -> String
showVersion (Version v) = intercalate "." (show <$> v)
