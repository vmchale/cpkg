{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , BuildVars (..)
                            , EnvVar (..)
                            , Command (..)
                            ) where

import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Dhall
import           GHC.Natural                      (Natural)
import           Package.C.Triple.Type
import           Package.C.Type.Shared
import           Package.C.Type.Version

data BuildVars = BuildVars { installDir   :: T.Text
                           , currentDir   :: T.Text
                           , targetTriple :: Maybe TargetTriple
                           , isCross      :: Bool
                           , includeDirs  :: [ T.Text ]
                           , preloadLibs  :: [ T.Text ]
                           -- TODO: nameToLinkDir function??
                           , shareDirs    :: [ T.Text ]
                           , linkDirs     :: [ T.Text ]
                           , binDirs      :: [ T.Text ]
                           , buildOS      :: OS
                           , buildArch    :: Arch
                           , static       :: Bool
                           , cpus         :: Natural
                           } deriving (Generic, Inject)

data EnvVar = EnvVar { var :: T.Text, value :: T.Text }
            deriving (Generic, Interpret)

data Command = CreateDirectory { dir :: T.Text }
             | MakeExecutable { file :: T.Text }
             | Call { program     :: T.Text
                    , arguments   :: [T.Text]
                    , environment :: Maybe [EnvVar]
                    , procDir     :: Maybe T.Text
                    }
             | SymlinkBinary { file :: T.Text }
             | SymlinkManpage { file :: T.Text }
             | Symlink { tgt :: T.Text, linkName :: T.Text }
             | Write { contents :: T.Text, file :: T.Text }
             | CopyFile { src :: T.Text, dest :: T.Text }
             | Patch { patchContents :: T.Text }
             deriving (Generic, Interpret)

data CPkg = CPkg { pkgName          :: T.Text
                 , pkgVersion       :: [ Natural ]
                 , pkgUrl           :: T.Text
                 , pkgSubdir        :: T.Text
                 , pkgStream        :: Bool -- ^ Use @tar@ package to stream
                 , pkgBuildDeps     :: [ Dep ] -- TODO: depend on target?
                 , pkgDeps          :: [ Dep ]
                 , configureCommand :: BuildVars -> [ Command ]
                 , buildCommand     :: BuildVars -> [ Command ]
                 , installCommand   :: BuildVars -> [ Command ]
                 -- TODO: add "description" field for printing
                 -- TODO: add "test" command for e.g. `make check`
                 } deriving (Generic, Interpret)

preDeps :: Doc a -> [ Dep ] -> Doc a
preDeps _ []   = ""
preDeps dep ds = hardline <> dep <+> hsep (punctuate "," (pretty . name <$> ds))

prettyDeps :: [ Dep ] -> Doc a
prettyDeps = preDeps "dependencies:"

prettyBldDeps :: [ Dep ] -> Doc a
prettyBldDeps = preDeps "build dependencies:"

instance Pretty CPkg where
    pretty (CPkg nam v url _ _ bds ds _ _ _) = pretty nam <##> indent 4 ("url:" <+> pretty url <##> "version:" <+> pretty (Version v) <> prettyDeps ds <> prettyBldDeps bds)
