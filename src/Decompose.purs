module Decompose where

-- functor isomorhpisms?
class Decompose f  g where
   decompose :: f a -> g a
   recompose :: g a -> f a
instance Decompose (Compose f g) (f g) where
  decompose (Compose x) = 