module Package.C.Triple ( -- * Types
                          TargetTriple (..)
                        , Arch (..)
                        , OS (..)
                        , Manufacturer (..)
                        , ABI (..)
                        -- * Parsers
                        , parseTriple
                        -- * Helper functions
                        , parseTripleIO
                        ) where

import           Package.C.Triple.Parse
import           Package.C.Triple.Type
