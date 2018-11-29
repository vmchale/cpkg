module System.Process.Ext ( waitProcess
                          ) where

import           Control.Monad            (void)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ask)
import           Package.C.Monad
import           Package.C.Type.Verbosity
import           System.Exit              (ExitCode (ExitSuccess), exitWith)
import           System.Process

handleExit :: ExitCode -> IO ()
handleExit ExitSuccess = mempty
handleExit x           = exitWith x

verbosityErr :: Verbosity -> StdStream
verbosityErr v | v >= Verbose = Inherit
verbosityErr _ = CreatePipe

waitProcess :: CreateProcess -> PkgM ()
waitProcess proc' = do
    v <- ask
    if v >= Loud
        then do
            (_, _, _, r) <- liftIO $ createProcess (proc' { std_out = Inherit, std_err = Inherit })
            liftIO (handleExit =<< waitForProcess r)
        else void $ liftIO $ readCreateProcess (proc' { std_err = verbosityErr v }) mempty

