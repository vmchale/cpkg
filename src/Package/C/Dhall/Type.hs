{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , ConfigureVars (..)
                            , BuildVars (..)
                            , InstallVars (..)
                            , EnvVar (..)
                            , Command (..)
                            ) where

import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Dhall
import           GHC.Natural                      (Natural)
import           Package.C.Type.Shared

data ConfigureVars = ConfigureVars { installDir   :: T.Text
                                   , targetTriple :: Maybe T.Text
                                   , includeDirs  :: [ T.Text ]
                                   , linkDirs     :: [ T.Text ]
                                   , binDirs      :: [ T.Text ]
                                   , configOS     :: OS
                                   , static       :: Bool
                                   } deriving (Generic, Inject)

data InstallVars = InstallVars { installPath :: T.Text
                               , installOS   :: OS
                               }
                    deriving (Generic, Inject)

data BuildVars = BuildVars { cpus    :: Natural
                           , buildOS :: OS
                           }
                deriving (Generic, Inject)

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
             | Write { contents :: T.Text, file :: T.Text }
             deriving (Generic, Interpret)

data CPkg = CPkg { pkgName          :: T.Text
                 , pkgVersion       :: [ Natural ]
                 , pkgUrl           :: T.Text
                 , pkgSubdir        :: T.Text
                 , pkgBuildDeps     :: [ Dep ]
                 , pkgDeps          :: [ Dep ]
                 , configureCommand :: ConfigureVars -> [ Command ]
                 , buildCommand     :: BuildVars -> [ Command ]
                 , installCommand   :: InstallVars -> [ Command ]
                 -- TODO: add "description" field for printing
                 } deriving (Generic, Interpret)

prettyDeps :: [ Dep ] -> Doc a
prettyDeps [] = ""
prettyDeps ds = hardline <> "dependencies:" <+> hsep (punctuate "," (pretty . name <$> ds))

instance Pretty CPkg where
    pretty (CPkg nam v url _ _ ds _ _ _) = pretty nam <##> indent 4 ("url:" <+> pretty url <##> "version:" <+> pretty v <> prettyDeps ds)
