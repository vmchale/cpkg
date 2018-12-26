{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Package.C.Type.Version ( Version (..)
                              , showVersion
                              ) where

import           CPkgPrelude
import           Data.List   (intercalate)

newtype Version = Version [ Natural ]
    deriving (Eq, Ord, Interpret, Binary, Hashable)

showVersion :: Version -> String
showVersion (Version v) = intercalate "." (show <$> v)

instance Pretty Version where
    pretty (Version v) = fold (punctuate "." (pretty <$> v))
