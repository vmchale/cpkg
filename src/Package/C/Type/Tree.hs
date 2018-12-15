{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Package.C.Type.Tree ( DepTree (..)
                           ) where

import           Control.Recursion

data DepTree p = Dep p [DepTree p]
               | BuildDep p [DepTree p]

data DepTreeF p x = DepF p [x]
                  | BuildDepF p [x]
                  deriving (Functor, Foldable, Traversable)

type instance Base (DepTree a) = DepTreeF a

instance Recursive (DepTree a) where
    project (Dep p ps)      = DepF p ps
    project (BuildDep p ps) = BuildDepF p ps
