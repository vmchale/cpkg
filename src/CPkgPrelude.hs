module CPkgPrelude ( -- * Base reëxports
                     Generic
                   , Natural
                   , void
                   , when
                   , unless
                   , traverse_
                   , fold
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
                   ) where

import           Control.Composition
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Binary               (Binary)
import           Data.Foldable
import           Data.Text.Prettyprint.Doc
import           Dhall
import           GHC.Generics              (Generic)
import           GHC.Natural               (Natural)
import           Lens.Micro