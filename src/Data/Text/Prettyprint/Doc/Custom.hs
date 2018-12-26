{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Prettyprint.Doc.Custom ( (<#>)
                                        , (<##>)
                                        , vdisplay
                                        , dashed
                                        ) where

import           Data.Foldable             (foldl')
import           Data.Text.Prettyprint.Doc

infixr 5 <#>
infixr 5 <##>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) a b = a <> line <> b

(<##>) :: Doc a -> Doc a -> Doc a
(<##>) a b = a <> hardline <> b

vdisplay :: [Doc a] -> Doc a
vdisplay = foldl' (<#>) mempty

dashed :: [Doc a] -> Doc a
dashed = concatWith (\x y -> x <> "-" <> y)
