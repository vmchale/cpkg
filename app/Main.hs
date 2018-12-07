module Main (main) where

import           Control.Monad       (void, when)
import           Data.Semigroup
import qualified Data.Text           as T
import qualified Data.Version        as V
import           Options.Applicative hiding (auto)
import           Package.C
import qualified Paths_cpkg          as P
import           System.Directory    (doesDirectoryExist, removeDirectoryRecursive)

cpkgVersion :: V.Version
cpkgVersion = P.version

data Command = Install { _pkgName :: String, _verbosity :: Verbosity, _target :: Maybe Platform }
             | Check { _dhallFile :: String, _verbosity :: Verbosity }
             | CheckSet { _dhallFile :: String, _verbosity :: Verbosity }
             | Dump { _pkgName :: String, _host :: Maybe Platform }
             | List
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
    (command "install" (info install (progDesc "Install a package from the global package set"))
    <> command "check" (info check (progDesc "Check a Dhall expression to ensure it can be used to build a package"))
    <> command "check-set" (info checkSet (progDesc "Check a package set defined in Dhall"))
    <> command "dump" (info dump (progDesc "Display flags to link against a particular library"))
    <> command "list" (info (pure List) (progDesc "List all available packages"))
    <> command "nuke" (info (pure Nuke) (progDesc "Remove all globally installed libraries"))
    )

ftypeCompletions :: String -> Mod ArgumentFields a
ftypeCompletions ext = completer . bashCompleter $ "file -X '!*." ++ ext ++ "' -o plusdirs"

dhallCompletions :: Mod ArgumentFields a
dhallCompletions = ftypeCompletions "dhall"

install :: Parser Command
install = Install
    <$> argument str
        (metavar "PACKAGE"
        <> help "Name of package to install")
    <*> verbosity
    <*> target

check :: Parser Command
check = Check <$> dhallFile <*> verbosity

checkSet :: Parser Command
checkSet = CheckSet <$> dhallFile <*> verbosity

target :: Parser (Maybe Platform)
target = optional
    (strOption
    (metavar "TARGET"
    <> long "target"
    <> help "Host platform, e.g. arm-linux-gnueabihf"
    ))

dump :: Parser Command
dump = Dump <$>
    argument str
    (metavar "PACKAGE"
    <> help "Name of package you want to link against"
    )
    <*> target

dhallFile :: Parser String
dhallFile =
    argument str
    (metavar "EXPRESSION"
    <> help "File containing a Dhall expression"
    <> dhallCompletions
    )

run :: Command -> IO ()
run (Install pkId v host') =
    runPkgM v $ buildByName (T.pack pkId) host'
run (Check file v) = void $ getCPkg v file
run (CheckSet file v) = void $ getPkgs v file
run (Dump name host) = printFlags name host
run Nuke = do
    pkgDir <- globalPkgDir
    exists <- doesDirectoryExist pkgDir
    when exists $
        removeDirectoryRecursive pkgDir
run List = displayPackageSet

main :: IO ()
main = execParser wrapper >>= run
