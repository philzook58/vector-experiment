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
-- foldMap (\n -> "type V" <> show (2*n) <> " a = (Compose V2 V" <> show n <> ") a ") $ map (\n -> pow 2 n) (1 .. 10)
-- 
type V4 a = (Compose V2 V2) a
type V8 a = (Compose V2 V4) a 
type V16 a = (Compose V2 V8) a 
type V32 a = (Compose V2 V16) a 
type V64 a = (Compose V2 V32) a 
type V128 a = (Compose V2 V64) a 
type V256 a = (Compose V2 V128) a 
type V512 a = (Compose V2 V256) a 
type V1024 a = (Compose V2 V512) a 
type V2048 a = (Compose V2 V1024) a 

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




-- foldMap (\n -> "type M" <> show (2*n) <> " a = (Compose M2 M" <> show n <> ") a ") $ map (\n -> pow 2 n) (1 .. 10)
data M2 a = M2 a a a a

type M4 a = (Compose M2 M2) a 
type M8 a = (Compose M2 M4) a 
type M16 a = (Compose M2 M8) a 
type M32 a = (Compose M2 M16) a 
type M64 a = (Compose M2 M32) a 
type M128 a = (Compose M2 M64) a 
type M256 a = (Compose M2 M128) a 
type M512 a = (Compose M2 M256) a 
type M1024 a = (Compose M2 M512) a 
type M2048 a = (Compose M2 M1024) a


-- alternative formaultion
type N2 f a = (Compose f f) a
type N3 f a = (Compose f N2) a

type V4' a = N2 V2 a
type V8' a = N3 V2 a



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

-- https://graphics.stanford.edu/~seander/bithacks.html
-- morton ordering
class ZOrder a where
  zorder :: Int -> Int -> a
  zorder :: (List Boolean) -> (List Boolean) -> a
  unzorder :: a -> Tuple (List Boolean) (List Boolean)

instance recursiveZOrder :: ZOrder a => ZOrder (Tuple (Tuple Boolean Boolean) a) where
   zorder x y = Tuple box (zorder (shr x 1) (shr y 1)) where
                                          box = Tuple xbit ybit
                                          xbit = intBit x
                                          ybit = intBit y
   unzorder (Tuple y@(Tuple a b) x) = (Tuple (i' : i) (j' : j))  where
                                                   Tuple i j = unzorder x
                                                   i' = a
                                                   j' = b
                                                   --multiplier = Cardinality a


-- The FFT obviously prefers that we index with lowest bit first
-- splitting blocks by even odd decopmpsition
-- but it is useful for other things too.
-- makes tridiagonal matrcies rescruisviyl triadiagonal
-- makes triangular recursively triagnluar

-- FFT suggests that perhaps circulant matrices are also block circulant.
-- Looks like it

class Toeplitz f g where
   toeplitz :: f -> g

toeplitz (V2 x y) = M2 (toeplitz x) (toeplitz y) (toeplitz y) (toeplitz x)

class Diag f g where
  diag :: f -> g

diag (V2 x y) = M2 (diag x) zero zero (diag y)

class Circulant f g where
  circ :: f -> g

newtype Circulant f a = Circulant (f a)

class Onto where
  to :: 

instance Dottable (Circulant V2 a) (M2 a) (M2 a)

instance Dottable (Circulant V2 a) (Circulant V2 a) (Circulant V2 a) where
  dot = --use FFT, 


circ x = toeplitz x (reverse x)

class Transposable f where
  transpose (M2 a b c d) = M2 (tranpose a) (transpose c) (transpose b) (transpose d)

-- one way to encode
data M2' tag a = M2' a a a a 
data Triangular
data Banded

-- or basically equvialently
newtype Tri a = Tri (M2 a)
-- Tri' for lower triangular?
newtype Circ a = Circ (M2 a)

-- Both cases have lots of obvious helpful boys
-- circulant is described by a single vector
-- diagonal blocks are identical
-- off diagonal are weird manipulations of each other
-- tridiagonal is half zeros. - Can be improved with Free
-- banded is composed of banded


-- I'm not positive I'm right still
-- conjecture: one submatrix has zeros on the diagonals
-- n^2/2 + n/2 is number of nonozero which ahs to be conserved
-- 3 *(n^2/4/2 + n/2/2) + (n^2/4/2 - n/2/2) if what I said is true
-- works for 4x4 case

-- triangular instances have a different inverse.
-- I'm not sure I even need to enforce that lower layers are triangular too
-- in the sense of
-- instance (Triangular a) => Division Ring (Tri a)
-- Necessary or not?
data BaseTri a = BaseTri a a a
data Symmettric a = Sym a a a
-- because then we'll use a pretty decent seeming blockwise triangular inversion

-- LU Deocmpostion , not sure if constraint is necessary
class Triangular l, Triangular u, Semiring m, Semiring u, Semiring l <= LU m l u | m -> l u where
   lu :: m -> Tuple l u

instance Triangular m, Semiring m => LU m m m where
   lu m = Tuple one m 
instance LU m l u => Semiring m => LU (M2 m) (Tri l) (Tri u) where -- use Dottable since we don't need lb ub
   lu (M2 a b c d) = Tuple (Tri (M2 one b * dinv zero one)) (Tri (M2 schur zero zero d)) -- Kind of except we need to keep l on left and b on right
                      Tuple la ua = lu a
                      Tuple lb ub = lu b
                      Tuple lc uc = lu c
                      Tuple ld ud = lu d
-- schur formula plus interleaving.
instance Dottable a b a => Dottable (M2 a) (Tri b) (M2 a)




zorder :: Int -> Int -> Int
zorder x y | x == 0 && y == 0 = 0 
zorder x y  = (mod x 2) + 2 * (mod y 2) + 4 * (zorder (shr x 1) (shr y 1))

unzorder :: Int -> Tuple Int Int
unzorder z = Tuple (z mod 2) (z mod 4)    

bits :: Int -> List Bit
ints :: List Bits -> Int
-- Functor instance for Pair a a
applypair f (Tuple x y) = Tuple (f x) (f y)

splitlist (x : y : zs) = Tuple (x : xs) (y : ys) where
                                          Tuple xs ys = splitlist zs


zorder = applypair $ splitListist <<< bits 


--data FreeSemiRing a = Pure a | One | Zero | Add (SemiRingMemo a) (SemiRingMemo a) | Mul (SemiRingMemo a) (SemiRingMemo a)
data FreeSemiRing a = One | Zero | Add a (SemiRingMemo a) | Mul a (SemiRingMemo a)
--data FreeSemiRing f a = Pure (f a) | One | Zero | Add (SemiRingMemo f a) (SemiRingMemo f a) | Mul (SemiRingMemo f a) (SemiRingMemo f a)
-- Now this is a pure semiring functor, that automatically makes something a semiring with no particular assumptions about a.
-- instance free
-- if a is a semiring also, we can easily interpet all of this in an bovious manner
-- Might just want this. Since often would rather just perform additions and multiplications on base type then keep expanded guy around
data FreeSemiRing' a = Pure' a | One | Zero 

-- Similar to hoe List = FreeMonoid
-- This has two kinds of Nil, One and Zero, and two Cons, Add and Mul
-- might conceivably attemmpt to support factoring

-- SemiRingSignature a = Add a a | Mul a a | One | Zero
-- FreeSemiRing = Free SemiRingSignature

type CPSFreeSemiRing a = forall b. (SemiRing b) => (a -> b) -> b 

{-
data HList x xs = HList x xs

class Zip ass bss as bs a b | a b as bs -> ass bss where
  zip :: ass -> bss -> TTuple  ??????

instance Zip as bs => Zip (Tuple a as) (Tuple b bs) where
   zip (Tuple a as) (Tuple b bs) = Tuple (Tuple a b) (zip as bs)
instance Zip Unit Unit where
   zip _ _ = unit

instance UnZip xs => UnZip (Tuple (Tuple a b) xs) where
   unzip (Tuple (Tuple a b) xs) = Tuple (Tuple a as) (Tuple b bs) where
                                                     Tuple as bs = unzip xs
-}

--unzorder gives bits


{-
class Zip f where
  zip :: f a -> f a ->  
  unzip
-}

instance baseZOrder :: ZOrder (Tuple Boolean Boolean) where
   zorder x y = box where
                  box = Tuple xbit ybit
                  xbit = intBit x
                  ybit = intBit y
   unzorder (Tuple a b) = Tuple (fromEnum a) (fromEnum b)


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

instance SemiRing1 f => SemiRing a => SemiRing (f a) where
  add = add1
  zero = zero1
  mul = mul1
  one = one1

instance Semiring a => SemiRing (f a) => SemiRing1 -- Yeah. Wait. I'm not sure SemiRing1 is a necessary thing at all

instance Semirring1 f, SemirRing1 g, => Semirign (Compose f g)
  add1 = Compose <<< add1 <<< unwwrap -- since g a is semiring via above 
-- so we only have to write one instance

--none of these things save us because InveRepresentable 
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
-- Metric1 ? Dottable? It is the predicate that there is a sensible notion of matrix multiply
class Metric p g f | p g -> f where
  mtabulate :: forall a. Semiring a => (g a -> f a) -> p a
  mindex :: forall a. Semiring a => p a -> (g a -> f a) 

instance metricM2 :: Metric M2 V2 V2 where
  mindex (M2 a b c d) (V2 x y) = V2 (a*x+b*y) (c*x+d*y)
  mtabulate f = M2 a b c d where
                           V2 a c = f (V2 one zero)
                           V2 b d = f (V2 zero one)

instance matrixMetric :: Metric M2 M2 M2 where
  mindex x y = x * y
  mtabulate f = f one
{-
   -- I don't think there is a way to write this wihtout SemiRing1. HOigher Order instance forall a. SemiRing a => (SemiRing a => SemiRing f a) => Metric f f f
instance SemiRing1 f => Metric f f f where
  mindex x y = x * y
  mtabulate f = f one

instance Applicative f, Foldable f => Metric f f Id
 mindex x y = fold (+) $ liftA2 (*) x y
 mtabulate = 

instance f Id (Dual f)
instance f Id f
-- this gives a way to convert to dual
instance f f Id

newtype LinOp f g a = LinOp (f a -> g a)
instance Metric (LinOp f g) f g where
  mindex = unwrap
  mtabulate = LinOp


class Metric1 f g h
class Metric a b c where

SemiRing a => Metric (M2 a) ... 


-}

--If this is dottable then we can use it to specify rectangular matrix dottability
{-
data Blocky a b c d x = Blocky (a x) (b x) (c x) (d x)

instance Metric a b b, Metric b c d, Metric c b a, Metric d b b, Metric a c c, ..., Semiring a, SemiRing d, Semiring x => Semiring (Blocky a b c d x) where
b' = mindex b
b'' = flip mindex b
-}

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


class DeCompose where 