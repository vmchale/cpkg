{-# LANGUAGE OverloadedStrings #-}

module Package.C.Error ( printErr
                       , unrecognized
                       , PackageError (..)
                       ) where

import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Exit

infixr 5 <#>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) a b = a <> line <> b

newtype PackageError = Unrecognized String

instance Pretty PackageError where
    pretty (Unrecognized t) = "Error: Unrecognized archive format when unpacking" <#> hang 2 (pretty t) <> hardline

printErr :: MonadIO m => PackageError -> m a
printErr e = liftIO (putDoc (pretty e) *> exitFailure)

unrecognized :: MonadIO m => String -> m a
unrecognized = printErr . Unrecognized
