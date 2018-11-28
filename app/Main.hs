module Main (main) where

import qualified Data.Version        as V
import           Dhall               (auto, inputFile)
import           Options.Applicative hiding (auto)
import           Package.C
import qualified Paths_cpkg          as P
import           System.Directory    (removeDirectoryRecursive)

cpkgVersion :: V.Version
cpkgVersion = P.version

data Command = Install { _dhallFile :: String }
             | Check { _dhallFile :: String }
             | Nuke

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
    <> command "nuke" (info (pure Nuke) (progDesc "Remove all globally installed libraries"))
    )

ftypeCompletions :: String -> Mod ArgumentFields a
ftypeCompletions ext = completer . bashCompleter $ "file -X '!*." ++ ext ++ "' -o plusdirs"

dhallCompletions :: Mod ArgumentFields a
dhallCompletions = ftypeCompletions "dhall"

install :: Parser Command
install = Install <$> dhallFile

check :: Parser Command
check = Check <$> dhallFile

dhallFile :: Parser String
dhallFile =
    argument str
    (metavar "EXPRESSION"
    <> help "File containing a Dhall expression"
    <> dhallCompletions
    )

run :: Command -> IO ()
run (Install file) = do
    unistring <- cPkgDhallToCPkg <$> inputFile auto file
    runPkgM Loud (buildCPkg unistring)
run (Check file) = do
    (_ :: CPkg) <- cPkgDhallToCPkg <$> inputFile autoFile
    mempty
run Nuke = do
    pkgDir <- globalPkgDir
    removeDirectoryRecursive pkgDir

main :: IO ()
main = execParser wrapper >>= run
