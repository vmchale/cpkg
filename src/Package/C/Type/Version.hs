{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Package.C.Type.Version ( Version (..)
                              , showVersion
                              ) where

import           Data.Binary               (Binary)
import           Data.Foldable             (fold)
import           Data.Hashable             (Hashable)
import           Data.List                 (intercalate)
import           Data.Text.Prettyprint.Doc
import           Dhall

newtype Version = Version [ Natural ]
    deriving (Eq, Ord, Interpret, Binary, Hashable)

showVersion :: Version -> String
showVersion (Version v) = intercalate "." (show <$> v)

instance Pretty Version where
    pretty (Version v) = fold (punctuate "." (pretty <$> v))
