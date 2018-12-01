module Main (main) where

import           Control.Monad       (void, when)
import           Data.Semigroup
import qualified Data.Version        as V
import           Options.Applicative hiding (auto)
import           Package.C
import qualified Paths_cpkg          as P
import           System.Directory    (doesDirectoryExist, removeDirectoryRecursive)

cpkgVersion :: V.Version
cpkgVersion = P.version

data Command = Install { _dhallFile :: String, _verbosity :: Verbosity }
             | Check { _dhallFile :: String, _verbosity :: Verbosity }
             | Nuke

verbosityInt :: Parser Int
verbosityInt = length <$>
    many (flag' () (short 'v' <> long "verbose" <> help "Turn up verbosity"))

verbosity :: Parser Verbosity
verbosity = fmap intToVerbosity verbosityInt

intToVerbosity :: Int -> Verbosity
intToVerbosity 0 = Normal
intToVerbosity 1 = Verbose
intToVerbosity 2 = Loud
intToVerbosity 3 = Diagnostic
intToVerbosity _ = Normal

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> userCmd)
    (fullDesc
    <> progDesc "The cpkg build tool and package manager."
    <> header "cpkg - a build tool for C\nsee 'man cpkg' for more detailed help")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("atspkg version: " ++ V.showVersion cpkgVersion) (short 'V' <> long "version" <> help "Show version")

userCmd :: Parser Command
userCmd = hsubparser
    (command "install" (info install (progDesc "Install a package defined by a Dhall expression"))
    <> command "check" (info check (progDesc "Check a Dhall expression to ensure it can be used to build a package"))
    <> command "nuke" (info (pure Nuke) (progDesc "Remove all globally installed libraries"))
    )

ftypeCompletions :: String -> Mod ArgumentFields a
ftypeCompletions ext = completer . bashCompleter $ "file -X '!*." ++ ext ++ "' -o plusdirs"

dhallCompletions :: Mod ArgumentFields a
dhallCompletions = ftypeCompletions "dhall"

install :: Parser Command
install = Install <$> dhallFile <*> verbosity

check :: Parser Command
check = Check <$> dhallFile <*> verbosity

dhallFile :: Parser String
dhallFile =
    argument str
    (metavar "EXPRESSION"
    <> help "File containing a Dhall expression"
    <> dhallCompletions
    )

run :: Command -> IO ()
run (Install file v) = do
    unistring <- cPkgDhallToCPkg <$> getCPkg v file
    runPkgM v (buildCPkg unistring)
run (Check file v) = void $ getCPkg v file
run Nuke = do
    pkgDir <- globalPkgDir
    exists <- doesDirectoryExist pkgDir
    when exists $
        removeDirectoryRecursive pkgDir

main :: IO ()
main = execParser wrapper >>= run
