{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Version ( Version (..)
                         , showVersion
                         ) where

import           Data.List (intercalate)
import           Dhall

newtype Version = Version [ Natural ]
    deriving Interpret

showVersion :: Version -> String
showVersion (Version v) = intercalate "." (show <$> v)
