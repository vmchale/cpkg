module CPkgPrelude ( -- * Base reëxports
                     Generic
                   , Natural
                   , void
                   , when
                   , unless
                   , traverse_
                   , fold
                   , toList
                   , (<=<)
                   , MonadIO (..)
                   -- Dhall reëxports
                   , Inject
                   , Interpret
                   -- * Exports from "Control.Composition"
                   , (.****)
                   -- * Exports from "Data.Binary"
                   , Binary
                   -- prettyprinter reëxports
                   , Pretty (..)
                   , punctuate
                   -- * microlens reëxports
                   , Lens'
                   , over
                   -- * Exports from "System.FilePath"
                   , (</>)
                   -- * Exports from "System.Directory"
                   , doesFileExist
                   , getAppUserDataDirectory
                   ) where

import           Control.Composition       ((.****))
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Binary               (Binary)
import           Data.Foldable
import           Data.Text.Prettyprint.Doc
import           Dhall                     (Inject, Interpret)
import           GHC.Generics              (Generic)
import           GHC.Natural               (Natural)
import           Lens.Micro                (Lens', over)
import           System.Directory
import           System.FilePath           ((</>))
