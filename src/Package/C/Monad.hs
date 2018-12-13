module Package.C.Monad ( PkgM
                       , runPkgM
                       -- * Logging
                       , putNormal
                       , putDiagnostic
                       ) where

import           Control.Monad.Reader
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
type PkgM = ReaderT Verbosity IO

runPkgM :: Verbosity -> PkgM a -> IO a
runPkgM = flip runReaderT
