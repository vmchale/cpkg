{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Package.C.Type.Tree ( DepTree (..)
                           , DepTreeF (..)
                           , asBldDep
                           ) where

import           Control.Recursion

data DepTree p = DepNode p [DepTree p]
               | BldDepNode p [DepTree p]
    deriving (Functor, Foldable, Traversable)

data DepTreeF p x = DepNodeF { self :: p, deps :: [x] }
                  | BldDepNodeF { self :: p, deps :: [x] }
                  deriving (Functor, Foldable, Traversable)

type instance Base (DepTree a) = DepTreeF a

instance Recursive (DepTree a) where
    project (DepNode p ps)    = DepNodeF p ps
    project (BldDepNode p ps) = BldDepNodeF p ps

asBldDep :: DepTree p -> DepTree p
asBldDep (DepNode p ps)    = BldDepNode p (fmap asBldDep ps)
asBldDep (BldDepNode p ps) = BldDepNode p (fmap asBldDep ps)
