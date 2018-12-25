{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Package.C.Monad ( PkgM
                       , MonadPkg
                       , runPkgM
                       ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Package.C.Db.Memory
import           Package.C.Db.Type
import           Package.C.Type.Verbosity

-- TODO: should this take a 'Maybe Platform' as well?
type PkgM = StateT InstallDb (ReaderT Verbosity IO)

type MonadPkg m = (MonadState InstallDb m, MonadReader Verbosity m, MonadIO m)

runPkgM :: Verbosity -> PkgM a -> IO a
runPkgM v act = do
    pSet <- strictIndex
    flip runReaderT v $ evalStateT act pSet
