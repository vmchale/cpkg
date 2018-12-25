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

data DumpTarget = Linker { _pkgGet :: String }
                | Compiler { _pkgGet :: String }
                | PkgConfig { _pkgGet :: String }
                | IncludePath { _pkgGet :: String }
                | LibPath { _pkgGet :: String }

data Command = Install { _pkgName :: String, _verbosity :: Verbosity, _target :: Maybe Platform, _static :: Bool, _packageSet :: Maybe String }
             | Check { _dhallFile :: String, _verbosity :: Verbosity }
             | CheckSet { _dhallFile :: String, _verbosity :: Verbosity }
             | Dump { _dumpTarget :: DumpTarget, _host :: Maybe Platform }
             | List { _packageSet :: Maybe String }
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

dumpTarget :: Parser DumpTarget
dumpTarget = hsubparser
    (command "linker" (info (Linker <$> package) (progDesc "Dump linker flags for a package"))
    <> command "compiler" (info (Compiler <$> package) (progDesc "Dump compiler flags for a package"))
    <> command "pkg-config" (info (PkgConfig <$> package) (progDesc "Dump pkg-config path for a package")) -- TODO: make pkg-config recursive or something?
    <> command "include" (info (IncludePath <$> package) (progDesc "Dump C_INCLUDE_PATH for a package"))
    <> command "library" (info (LibPath <$> package) (progDesc "Dump LD_LIBRARY_PATH or LIBRARY_PATH for a package"))
    )

userCmd :: Parser Command
userCmd = hsubparser
    (command "install" (info install (progDesc "Install a package from the global package set"))
    <> command "check" (info check (progDesc "Check a Dhall expression to ensure it can be used to build a package"))
    <> command "check-set" (info checkSet (progDesc "Check a package set defined in Dhall"))
    <> command "dump" (info dump (progDesc "Display flags to link against a particular library"))
    <> command "list" (info list (progDesc "List all available packages"))
    <> command "nuke" (info (pure Nuke) (progDesc "Remove all globally installed libraries"))
    )

list :: Parser Command
list = List <$> packageSet

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
    <*> static'
    <*> packageSet

packageSet :: Parser (Maybe String)
packageSet = optional
    (strOption
    (metavar "EXPRESSION"
    <> long "pkg-set"
    <> help "Dhall expression for the package set to be used"
    ))

static' :: Parser Bool
static' =
    switch
    (long "static"
    <> help "Build static libaries")

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

package :: Parser String
package =
    argument str
    (metavar "PACKAGE"
    <> help "Name of package you want to link against"
    <> completer (listIOCompleter allPackages)
    )

dump :: Parser Command
dump = Dump
    <$> dumpTarget
    <*> target

dhallFile :: Parser String
dhallFile =
    argument str
    (metavar "EXPRESSION"
    <> help "File containing a Dhall expression"
    <> dhallCompletions
    )

run :: Command -> IO ()
run (Install pkId v host' sta pkSet) =
    runPkgM v $ buildByName (T.pack pkId) host' pkSet sta
run (Check file v) = void $ getCPkg v file
run (CheckSet file v) = void $ getPkgs v file
run (Dump (Linker name) host) = runPkgM Normal $ printLinkerFlags name host
run (Dump (Compiler name) host) = runPkgM Normal $ printCompilerFlags name host
run (Dump (PkgConfig name) host) = runPkgM Normal $ printPkgConfigPath name host
run (Dump (IncludePath name) host) = runPkgM Normal $ printIncludePath name host
run (Dump (LibPath name) host) = runPkgM Normal $ printLibPath name host
run Nuke = do
    pkgDir <- globalPkgDir
    exists <- doesDirectoryExist pkgDir
    when exists $
        removeDirectoryRecursive pkgDir
run (List pkSet) = displayPackageSet pkSet

main :: IO ()
main = execParser wrapper >>= run
