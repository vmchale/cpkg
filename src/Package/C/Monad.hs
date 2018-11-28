module Package.C.Monad ( PkgM
                       , runPkgM
                       -- * Logging
                       , putNormal
                       , putDiagnostic
                       ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Package.C.Type

putVerbosity :: Verbosity -> String -> PkgM ()
putVerbosity verb s = do
    v <- ask
    when (v >= verb)
        (liftIO (putStrLn s))

putNormal :: String -> PkgM ()
putNormal = putVerbosity Normal

putDiagnostic :: String -> PkgM ()
putDiagnostic = putVerbosity Diagnostic

type PkgM = ReaderT Verbosity IO

runPkgM :: Verbosity -> PkgM a -> IO a
runPkgM = flip runReaderT
