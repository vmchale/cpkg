{-# LANGUAGE FlexibleContexts #-}

module Package.C.Logging ( putNormal
                         , putDiagnostic
                         ) where

import           Control.Monad.Reader
import           Package.C.Type.Verbosity

putVerbosity :: (MonadReader Verbosity m, MonadIO m) => Verbosity -> String -> m ()
putVerbosity verb s = do
    v <- ask
    when (v >= verb)
        (liftIO (putStrLn s))

putNormal :: (MonadReader Verbosity m, MonadIO m) => String -> m ()
putNormal = putVerbosity Normal

putDiagnostic :: (MonadReader Verbosity m, MonadIO m) => String -> m ()
putDiagnostic = putVerbosity Diagnostic
