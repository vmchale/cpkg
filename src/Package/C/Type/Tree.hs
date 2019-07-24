{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Package.C.Type.Tree ( DepTree (..)
                           , DepTreeF (..)
                           , asBldDep
                           ) where

import           Control.Recursion
import           GHC.Generics      (Generic)

data DepTree p = DepNode p Bool [DepTree p]
               | BldDepNode p [DepTree p]
    deriving (Functor, Foldable, Traversable, Generic, Recursive)

data DepTreeF p x = DepNodeF { self :: p, man :: Bool, deps :: [x] }
                  | BldDepNodeF { self :: p, deps :: [x] }
                  deriving (Functor, Foldable, Traversable, Generic)

type instance Base (DepTree a) = DepTreeF a

asBldDep :: DepTree p -> DepTree p
asBldDep (DepNode p _ ps)  = BldDepNode p (fmap asBldDep ps)
asBldDep (BldDepNode p ps) = BldDepNode p (fmap asBldDep ps)
