{-# LANGUAGE ConstraintKinds #-}

module Package.C.Db.Monad ( MonadDb
                          ) where

import           Control.Monad.State
import           Package.C.Db.Type

type MonadDb = MonadState InstallDb
