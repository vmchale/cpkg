module Package.C.Monad ( PkgM
                       , runPkgM
                       -- * Logging
                       , putNormal
                       , putDiagnostic
                       ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Package.C.Db.Memory
import           Package.C.Db.Type
import           Package.C.Type.Verbosity

putVerbosity :: Verbosity -> String -> PkgM ()
putVerbosity verb s = do
    v <- ask
    when (v >= verb)
        (liftIO (putStrLn s))

putNormal :: String -> PkgM ()
putNormal = putVerbosity Normal

putDiagnostic :: String -> PkgM ()
putDiagnostic = putVerbosity Diagnostic

-- TODO: should this take a 'Maybe Platform' as well?
type PkgM = StateT InstallDb (ReaderT Verbosity IO)

runPkgM :: Verbosity -> PkgM a -> IO a
runPkgM v act = do
    pSet <- strictIndex
    flip runReaderT v $ evalStateT act pSet
