{-# LANGUAGE OverloadedStrings #-}

module Package.C.Error ( printErr
                       , unrecognized
                       , indexError
                       , corruptedDatabase
                       , unfoundPackage
                       , PackageError (..)
                       ) where

import           CPkgPrelude
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Exit

data PackageError = Unrecognized String
                  | IndexError String -- package name
                  | CorruptedDatabase
                  | UnfoundPackage -- TODO: this should take the package name as an argument

instance Pretty PackageError where
    pretty (Unrecognized t)  = "Error: Unrecognized archive format when unpacking" <#> hang 2 (pretty t) <> hardline
    pretty (IndexError str)  = "Error: Package" <+> pretty str <+> "not found in your indices. Try 'cpkg install" <+> pretty str <> "'." <> hardline
    pretty CorruptedDatabase = "Error: Package database corrupted. Please try 'cpkg nuke'" <> hardline
    pretty UnfoundPackage    = "Error: Package not found" <> hardline

printErr :: MonadIO m => PackageError -> m a
printErr e = liftIO (putDoc (pretty e) *> exitFailure)

unrecognized :: MonadIO m => String -> m a
unrecognized = printErr . Unrecognized

indexError :: MonadIO m => String -> m a
indexError = printErr . IndexError

corruptedDatabase :: MonadIO m => m a
corruptedDatabase = printErr CorruptedDatabase

unfoundPackage :: MonadIO m => m a
unfoundPackage = printErr UnfoundPackage
