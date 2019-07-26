module CPkgPrelude ( -- * Base reëxports
                     Generic
                   , Natural
                   , void
                   , when
                   , unless
                   , traverse_
                   , fold
                   , toList
                   , filterM
                   , ($>)
                   , (<=<)
                   , (<=*<)
                   , MonadIO (..)
                   , Void
                   -- * Dhall reëxports
                   , Inject
                   , Interpret
                   -- * hashable reëxports
                   , Hashable
                   -- * Exports from "Data.Binary"
                   , Binary
                   -- prettyprinter reëxports
                   , Doc
                   , Pretty (..)
                   , punctuate
                   -- * microlens reëxports
                   , Lens'
                   , over
                   -- * Exports from "System.FilePath"
                   , (</>)
                   -- * Exports from "System.Directory"
                   , doesFileExist
                   , removeDirectoryRecursive
                   , getAppUserDataDirectory
                   ) where

import           Control.Composition       ((<=*<))
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Binary               (Binary)
import           Data.Foldable
import           Data.Functor              (($>))
import           Data.Hashable             (Hashable)
import           Data.Text.Prettyprint.Doc
import           Data.Void                 (Void)
import           Dhall                     (Inject, Interpret)
import           GHC.Generics              (Generic)
import           GHC.Natural               (Natural)
import           Lens.Micro                (Lens', over)
import           System.Directory
import           System.FilePath           ((</>))
