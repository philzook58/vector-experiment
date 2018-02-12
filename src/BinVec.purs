module BinVec where


import Prelude 
import Vec
import Data.Tuple
import Data.Identity
import Data.Newtype
import Data.Int.Bits
import Data.Maybe (fromMaybe)
import Data.Enum

data V2 a = V2 a a

instance functorV2 :: Functor V2 where
   map f (V2 x y) = V2 (f x) (f y)

instance vefFRep :: Representable V2 Boolean where
   tabulate f = V2 (f false) (f true)
   index (V2 x y) b = if b then y else x

instance showV2 :: Show a => Show (V2 a) where
   show (V2 x y) = "V2 " <> show x <> " " <> show y

instance semiringVec :: (Semiring a) => Semiring (V2 a) where
   add (V2 x y) (V2 a b) = V2 (add x a) (add y b)
   zero = V2 zero zero
   mul (V2 x y) (V2 a b) = V2 (x * a) (y * b) --elementwise mltiplication
   one = V2 one one



data M2 a = M2 a a a a

instance functorM2 :: Functor M2 where
   map f (M2 x y z w) = M2 (f x) (f y) (f z) (f w)

instance representableM2 :: Representable M2 (Tuple Boolean Boolean) where
   tabulate f = M2 (f' false false) (f' false true) (f' true false) (f' true true) where f' = curry f
   index (M2 x y z w) (Tuple row col) = if row then
                                                   if col then w else z
                                               else
                                                   if col then y else x

{-

zorder :: Int -> Int ->  Tuple Boolean Boolean
zorder x y = if (x < 2 && y < 2) then box else Tuple box (zorder (shl x 1) (shr y 1)) where
                                          box = Tuple xbit ybit
                                          xbit = fromMaybe false $ toEnum (mod x 2) 
                                          ybit = fromMaybe false $ toEnum (mod y 2)
-}
intBit x = fromMaybe false $ toEnum (mod x 2)


mK i j | i == j - 1 = -1
mK i j | i == j + 1 = -1
mK i j | i == j = 2
mK _ _ = 0

class ZOrder a where
  zorder :: Int -> Int -> a
  unzorder :: a -> Tuple Int Int

instance recursiveZOrder :: ZOrder a => ZOrder (Tuple (Tuple Boolean Boolean) a) where
   zorder x y = Tuple box (zorder (shr x 1) (shr y 1)) where
                                          box = Tuple xbit ybit
                                          xbit = intBit x
                                          ybit = intBit y
   unzorder (Tuple y@(Tuple a b) x) = Tuple (Tuple a i)  where
                                                   Tuple i j = unzorder x


instance baseZOrder :: ZOrder (Tuple Boolean Boolean) where
   zorder x y = box where
                  box = Tuple xbit ybit
                  xbit = intBit y
                  ybit = intBit x


tabulate zorder
{-
   class Invariant p <= Invrepresentable p g f | p -> f g where
  itabulate :: forall a. (g a -> f a) -> p a 
  iindex :: forall a. p a -> (g a -> f a)  
  -}

{-
class Linear f a where

class LinearFunctor f where
   add1 ...

class Semiring1 f where
   add1 :: forall a. Semiring a => f a -> f a -> f a 
   zero1 :: forall a. Semiring a => f a
   mul1 :: forall a. Semiring a => f a -> f a -> f a 
   one1 :: forall a. Semiring a => f a
--none of these things save us
-}

newtype Dual f a = Dual (f a -> a)

instance dualMetric :: Metric (Dual f) f Identity  where
  mindex (Dual f) x = Identity $ f x
  mtabulate f = Dual $ unwrap <<< f

dot = mindex

{-
instance enumerableMetric :: (Representable f a, BoundedEnum a) => Metric f f Identity where
  mindex = Identity $ sum $ map (\x ->  (v x) * (w x)) basis
  mtabulate f = 
-}
{-
-- No, maybe not. Tuple does not have the same zazz as arrow
class Birepresentable p f g where
  bitabulate :: Tuple (g a) (f a) -> p a
  biindex :: p a -> Tuple (g a) (f a) 
-}
{-
instance composerMetric :: Metric p1 f1 g1, Metric p2 f2 g2 => Metric (Compose p1 p2) (Compose f1 f2) (Compose g1 g2) where
  mindex 
-}
-- Metric1 ?
class Metric p g f where
  mtabulate :: forall a. Semiring a => (g a -> f a) -> p a
  mindex :: forall a. Semiring a => p a -> (g a -> f a) 

instance metricM2 :: Metric M2 V2 V2 where
  mindex (M2 a b c d) (V2 x y) = V2 (a*x+b*y) (c*x+d*y)
  mtabulate f = M2 a b c d where
                           V2 a c = f (V2 one zero)
                           V2 b d = f (V2 zero one)
{-
   -- a true instance of Invrepresentable doesn't need semigroup
   -- an instance of profunctor doesn't need variables to be different
-- instance Prorepresentable p f g => Invrepresentable (Diag p) f g where  
-- instance Invrepresentable m f g => Metric m f g where
     mindex = iindex
     mtabulate = itabulate
-}

{-
   --doesn't work because of required SemiRing instance for a, whihch we have no handle on
instance invrepresentableM2 :: Semiring a => Invrepresentable M2 V2 V2 where
  iindex (M2 a b c d) (V2 x y) = V2 (a*x+b*y) (c*x+d*y)
  itabulate f = M2 a b c d where
                           V2 a c = f (V2 one zero)
                           V2 b d = f (V2 zero one)
-}

instance semiringM2 :: (Semiring a) => Semiring (M2 a) where
   add (M2 x y z w) (M2 a b c d) = M2 (x+a) (y+b) (z+c) (w+d)
   zero = M2 zero zero zero zero
   mul (M2 x y z w) (M2 a b c d) = M2 (x*a+y*c) (x*b+y*d) (z*a+w*c) (z*b+w*d)  -- Could use strassen
   one = M2 one zero zero one

