module MatProd where

import Prelude

-- The various verisons of suggested matrix product

class MatVec a v where
   matvec :: a -> v -> v

-- SemiRing is used for matrix matrix product

-- This is also used for kmett's linear
-- the intention is this is 
class Metric f where
  dot :: forall a. Semiring a => f a -> f a -> a

-- metric takes vector to dual 


-- And the new metric instance that came from adding a necessary SemiRing constraint (true vectors hold a single type of numberlike objects) to InvRepresentable
-- could be called dottable
-- 
class Metric p g f | p g -> f where
  mtabulate :: forall a. Semiring a => (g a -> f a) -> p a
  mindex :: forall a. Semiring a => p a -> (g a -> f a) 


-- Metric f f Id => MetricKmett f where
-- MetricKmett f => Metric f f Id
