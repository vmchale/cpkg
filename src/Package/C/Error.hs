{-# LANGUAGE OverloadedStrings #-}

module Package.C.Error ( printErr
                       , unrecognized
                       , badCommand
                       , PackageError (..)
                       ) where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Exit

infixr 5 <#>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) a b = a <> line <> b

data PackageError = Unrecognized String
                  | BadCommand

instance Pretty PackageError where
    pretty (Unrecognized t) = "Error: Unrecognized archive format when unpacking" <#> hang 2 (pretty t) <> hardline
    pretty BadCommand       = "Error: command must not be empty."

printErr :: PackageError -> IO a
printErr e = putDoc (pretty e) *> exitFailure

unrecognized :: String -> IO a
unrecognized = printErr . Unrecognized

badCommand :: IO a
badCommand = printErr BadCommand
