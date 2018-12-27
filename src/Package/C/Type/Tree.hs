{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Package.C.Type.Tree ( DepTree (..)
                           , DepTreeF (..)
                           , asBldDep
                           , asBldDepF
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
asBldDep (DepNode p ps) = BldDepNode p ps
asBldDep x@BldDepNode{} = x

asBldDepF :: DepTreeF p x -> DepTreeF p x
asBldDepF (DepNodeF p ps) = BldDepNodeF p ps
asBldDepF x@BldDepNodeF{} = x
