{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Type.Version ( Version (..)
                         , showVersion
                         ) where

import           Data.Hashable (Hashable)
import           Data.List     (intercalate)
import           Dhall

newtype Version = Version [ Natural ]
    deriving (Interpret, Hashable)

showVersion :: Version -> String
showVersion (Version v) = intercalate "." (show <$> v)
