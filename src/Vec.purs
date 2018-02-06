module Vec where

import Data.Array
import Data.DivisionRing
import Data.Either
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Newtype
import Data.Ring
import Data.Semiring
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Type.Proxy

import Control.Category (compose)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.CommutativeRing (zero)
import Data.Enum

import Data.Bifunctor
import Data.Functor.Product
import Data.Functor.Compose
import Data.Functor.Coproduct
import Data.Profunctor
import Data.Functor.Invariant

import Control.Monad.Free
import Partial.Unsafe (unsafePartial)
import Data.Foldable (sum)
--data Mat = MBlock Mat Mat Mat Mat | MNum Number

-- implementation makes assumtion that matrix is NxN of vector size
foreign import matVecArr :: forall x. (Sized x) => (AVec (Tuple x x) Number) -> (AVec x Number) -> (AVec x Number)

-- only powers of 2.
data MatF a = MBlock a a a a
data VecF a = VBlock a a

instance showVecF :: Show a => Show (VecF a) where
   show (VBlock x y) = "V2 " <> show x <> " " <> show y
-- VecF is the functor vector form of spin
type Spin a = VecF a
-- MatF is a terrible name
type BinVec a = VecF a

type V2 a = VecF a
type V4 a = Compose VecF VecF a
type V8 a = V4 (VecF a)
type V16 a = V8 (VecF a)
type V32 a = V16 (VecF a)
type V64 a = V32 (VecF a)
type V128 a = V64 (VecF a)
type V256 a = V128 (VecF a)
type V512 a = V256 (VecF a)
type V1024 a = V512 (VecF a)
type V2046 a = V1024 (VecF a)

type M2 a = MatF a
type M4 a = M2 (MatF a)
type M8 a = M4 (MatF a)
type M16 a = M8 (VecF a)
type M32 a = M16 (MatF a)
type M64 a = M32 (MatF a)
type M128 a = M64 (MatF a)
type M256 a = M128 (MatF a)
type M512 a = M256 (MatF a)
type M1024 a = M512 (MatF a)
type M2046 a = M1024 (MatF a)

type I2 = Boolean
type I4 = Tuple Boolean I2
type I8 = Tuple Boolean I4
type I16 = Tuple Boolean I8
type I32 = Tuple Boolean I16
type I64 = Tuple Boolean I32
type I128 = Tuple Boolean I64
type I256 = Tuple Boolean I128
type I512 = Tuple Boolean I256
type I1024 = Tuple Boolean I512
type I2046 = Tuple Boolean I1024


-- Can get values of this type using toEnum
-- and fromEnum

type FreeVec a = Free VecF a
type FreeMat a = Free MatF a

{-
--warmup
-- okay this is the fold...
sumV :: SemiRing a => FreeVec a -> a
sumV 
-}



class Functor f <= Representable f a | f -> a where
   tabulate :: forall b. (a -> b) -> f b
   index :: forall b. f b -> (a -> b)

instance fVec :: Functor VecF where
   map f (VBlock x y) = VBlock (f x) (f y)

instance vefFRep :: Representable VecF Boolean where
   tabulate f = VBlock (f false) (f true)
   index (VBlock x y) t = if t then y else x

instance repCompose :: (Representable f a, Representable g b) => Representable (Compose f g) (Tuple a b) where
   tabulate = Compose <<< tabulate <<< map tabulate <<< curry
   index (Compose fg) (Tuple i j) = index (index fg i) j


fillRange :: forall f a. BoundedEnum a => Representable f a => f Int
fillRange = tabulate (\x -> (fromEnum x))


{-
-- Need a Diag Class
diag :: Semiring a => Free VecF a -> Free MatF a
diag (VBlock x y) = MBlock (diag x) zero zero (diag y)
-}

class Profunctor p <= Prorepresentable p g f | p -> f g where
   ditabulate :: forall a b. (g a -> f b) -> p a b
   diindex :: forall a b. p a b -> (g a -> f b)

class Invariant p <= Invrepresentable p g f | p -> f g where
  itabulate :: forall a. (g a -> f a) -> p a 
  iindex :: forall a. p a -> (g a -> f a)  

-- You'd Have to use Compose, but I've said this before haven't I
instance Invrepresentable p g f, Invrepresentable q h j => Invrepresentable (Compose p q) (Compose g h) (Compose f j) where


instance Invrepresentable M2 V2 V2 where
   iitabulate = 




{-
      -- This is apparently not the correct notion of prorepresentable
class Profunctor p <= Prorepresentable p a b | p -> a b where
   ditabulate :: forall s t. ((a -> t) -> b -> s) -> p t s
   diindex :: forall s t. p t s -> ((a -> t) -> b -> s)

newtype StarCoStar f a b = SCS (f a -> f b)


instance profunctorscs :: Functor f => Profunctor (StarCoStar f) where
   dimap f g (SCS h) = SCS $ (map f) >>> h >>> (map g)

instance prorespesentableStarCoStar :: Representable f a => Prorepresentable (StarCoStar f) a a where
   ditabulate =
   diindex = 
-}
-- forall p. Prorepresentable p => p s t -> p a b 

-- RepresetnableProfunctor
-- ProRepresentable
-- VecF ~ Kron (Bool->Number) 

-- One possible solution: Prime Factorization of dimension (use 3x3,5x5 and so on block matrix) How to autogenerate these types? Could do with type families
-- Okay, one easy way to do prime factorization is to just index by finite types. Just sayin
-- Mat a b = (a,a)->b -- Mat as direct product of
-- Vec a b = a -> b

-- can we then autoderive stuff as easily?
-- Other possiblity: Zero padding -- easy, wasteful but within a factor of 2.
-- Or use array based unsafe as base case.

-- adds single row and column
-- data IndMat a b = MBlock Number b b a
-- insists on a MatVec a b instance as requirment for a ring.

--data MatF' a b c d = MBlock a b c d
data MatF'' a d = MBlock'' a (d -> a) (a -> d) d
-- d -> a is actually conjugation by B .... C
-- a -> d is oppposite conjugation C .... B
-- There is no way you can make a matrix vector product out of this
-- It does make a well typed matrix algebra though.
-- okay, you can pass one to d->a?
-- That allows you to view the product B * C
-- which you could take the Svd of.
-- This is silly. We can't possibly build a Ring instance for this

data MatF''' a d w v = MBlock''' a (v -> w) (w -> v) d
-- store recyangular matrices as LinOps, have to convert and deconvert a and d in order to compose
-- unless we define a new matrix composition

-- interesting
{-

-- Yes this works pure function vector style. Very interesting.

class Invertible a where
inv :: a -> a

instance arrowInv :: Invertible (LinOp a a), Invertible (LinOp c c) => Invertible (LinOp (Either a c) (Either a c)) where
inv f = MBlock a b c d = deblock f

The LinOp inversion actually may not build the matrix in memeory. It build the function that will perform solve(A,-). inv = curried solve
It also does fit low rank facotrizations into C and B
if C and B are 0, that can be very fast. const 0. Sparsity can be done with (ln n) overhead? That's pretty good.
All of this does not really rely on the Function Vector representation, more on the LinOp as function representation.
Could use Phantom Type on Array base case to aid type based recursion.

newtype AVec a n = AVec (Array n)

foreign import of typed float?

Most difficulty of using Nat indexed vectors goes away when you don't actually need type addition to work. You Can just Leave it as Either.

Use Dual vector?
Vec a -> Number

and DLinOp mostly?

for exmaple
data BoxLength

Vec BoxLength
or Vec (BoxLength,BoxLength,BoxLength)

block deblock is the isomorphism into blocks

-- alternatively for the 2^n thing
instance otherInv :: Invertible (LinOp (Bool, c) (Bool, c))

usingisomorphism

Either a a = (Bool, a)

-}

newtype AVec a n = AVec (Array n)

derive instance fV :: Functor (AVec n)
instance aV :: Apply (AVec n) where
   apply (AVec f) (AVec x) = AVec (apply f x)

instance a2V :: Sized n => Applicative (AVec n) where
   pure x = AVec (replicate (size (Proxy :: Proxy n)) x)
{-
instance a2V :: BoundedEnum n => Applicative (AVec n) where
   pure x = AVec (replicate (unwrap (cardinality :: Cardinality n)) x)
-}

class Sized x where
   size :: Proxy x -> Int

-- I'm conflating two things here?

-- BoundedEnum
-- size is called cardinality
-- Doesn't have a -> b instance strangely?

instance voidSize :: Sized Void where
   size _ = 1

instance numSize :: Sized Number where
   size _ = 1

instance boolSize :: Sized Boolean where
   size _ = 2

type Double a = Tuple Boolean a

instance eitherSize :: (Sized a, Sized b) => Sized (Either a b) where
   size _ = (size (Proxy :: Proxy a)) + (size (Proxy :: Proxy b))

instance tupleSize :: (Sized a, Sized b) => Sized (Tuple a b) where
   size _ = (size (Proxy :: Proxy a)) * (size (Proxy :: Proxy b))

instance arrSize :: (Sized a, Sized b) => Sized (a -> b) where
   size _ = pow (size (Proxy :: Proxy b)) (size (Proxy :: Proxy a))

instance maybeSize :: (Sized a) => Sized (Maybe a) where
   size _ = (size (Proxy :: Proxy a)) + 1

castVec :: forall n a b. Sized a => Sized b => AVec a n -> Maybe (AVec b n)
castVec (AVec x) = if (size (Proxy :: Proxy a)) == (size (Proxy :: Proxy b)) then Just (AVec x) else Nothing

--class Metric f n where
class Metric f where
  dot :: forall a. Semiring a => f a -> f a -> a




instance metricenumVec :: BoundedEnum a => Metric ((->) a) where
   dot v w = sum $ map (\x ->  (v x) * (w x)) basis




basis :: forall a. BoundedEnum a => Array a
basis = unsafePartial $ map (fromJust <<< toEnum) (range 0 $ unwrap (cardinality :: Cardinality a))


{-size' :: BoundedEnum a => Proxy a -> Int
size' _ = case cardinality of
               Cardinality x -> x 
-}
{-
instance aVecSize :: Sized b => Sized (AVec b a) where
length _ = length (Proxy :: Proxy b)
-}
-- Maybe it needs an SProxy type
{-
instance vecfSize :: Sized a => Sized (VecF a) where
size _ = 2 * (size (Proxy :: Proxy a) )
-}

instance semiringAVec :: (Semiring a, Sized x) => Semiring (AVec x a) where
   add (AVec x) (AVec y) = AVec $ zipWith add x y
   zero = pure zero
   mul (AVec x) (AVec y) = AVec $ zipWith mul x y
   one = pure one
{-
instance semiringBoundedEnumAVec :: (Semiring a, BoundedEnum x) => Semiring (AVec x a) where
   add (AVec x) (AVec y) = AVec $ zipWith add x y
   zero = pure zero
   mul (AVec x) (AVec y) = AVec $ zipWith mul x y
   one = pure one
-}
instance matVecAVec :: (Sized x) => MatVec (AVec (Tuple x x) Number) (AVec x Number) where
   matvec = matVecArr

{-
helper y:ys (x:xs) acc = helper ys xs (acc+y*x)
helper ys [] acc = acc
helper2 ys
unsafePartial $ unsafeIndex a
range (length (Proxy :: Proxy x))

-}

-- rectangular aren't Ring unfortunately
data VStack a b = VStack a b
data HStack c d = HStack c d

class Factors a b c | a b -> c where
   mult :: a -> b -> c

-- Factors b c a, Factors c b d, SemiRing a, SemiRing d =>
-- Forces them to be right next to each toher though, which isn't right. The conjugation idea is more correct
--class Conjugatable a b c d where
--  conj1 :: (b,c) -> a -> d -- produces d = c * a * b
--  conj2 :: (c,b) -> d -> a
-- when multipling matrix, we need to be ab;e to pick the b and c elements from different guys. That is why we can't store
-- this is still not enough.

-- A Low Rank Matrix
--data LowRank a = LowRank a m b

--data Factored = (a , b)

--data Vec = VBlock Vec Vec | VNum Number

instance semiringVec :: (Semiring a) => Semiring (VecF a) where
   add (VBlock x y) (VBlock a b) = VBlock (add x a) (add y b)
   zero = VBlock zero zero
   mul (VBlock x y) (VBlock a b) = VBlock (x * a) (y * b) --elementwise mltiplication
   one = VBlock one one

instance semiringMat :: (Semiring a) => Semiring (MatF a) where
   add (MBlock x y z w) (MBlock a b c d) = MBlock (x+a) (y+b) (z+c) (w+d)
   zero = MBlock zero zero zero zero
   mul (MBlock x y z w) (MBlock a b c d) = MBlock (x*a+y*c) (x*b+y*d) (z*a+w*c) (z*b+w*d)  -- Could use strassen
   one = MBlock one zero zero one

instance ringMat :: (Ring a) => Ring (MatF a) where
   sub (MBlock x y z w) (MBlock a b c d) = MBlock (x-a) (y-b) (z-c) (w-d)

instance divisionRingMat :: (DivisionRing a) => DivisionRing (MatF a) where
   recip (MBlock a b c d) = MBlock m               (m * b * dinv)
                                 (dinv * c * m)  (dinv * c * m * b * dinv + dinv) where
                                                                                    m = recip (a - b * dinv * c)
                                                                                    dinv = recip d
{-
basis :: forall n. Semiring a => Proxy n -> AVec n (AVec n a)
basis = undefined --snoc zero

-- almost a monad instance?
fuse :: forall n. AVec n (AVec n a) -> AVec (Tuple n n) a
fuse = undefined
-- almost a comonad instance?
rows :: forall n. AVec (Tuple n n) a -> AVec n (AVec n a)
rows = undefined
-}

--newtype LinOp a b c = LinOp (AVec a c -> AVec b c)
type LinOp'' a b c = AVec a c -> AVec b c
newtype LinOp' a b c d = LinOp' (AVec a c -> AVec b d)


data DSum a b = DSum a b


instance bifunnctorDSum :: Bifunctor DSum where
   bimap f g (DSum a b) = DSum (f a) (g b)
--newtype DSum a b = DSum (Tuple a b)
-- newtype DSum a b = DSum (Tuple a b)

-- should use Sequence, not array. Or List. Or Cat-List?
-- every addition does full copy
-- This is weird. DSum is basically tuple. We actually gain a couple things by using DSum here though.
-- It turns out it makes not so much sense to Kron with a scalar in there?
-- This Kron is in some sense "Free"
newtype Kron a b = Kron (Array (DSum a b))


instance bifunctorKron :: Bifunctor Kron where
   bimap f g (Kron x) = Kron $ map (bimap f g) x 
-- Partially applied Kron Lifts an ordinary datalike vector into a functor vector
type KronAVec a b c = Kron (AVec a c) b

type KronFunctorVector = Compose
type DSumFunctorVector = Product
type AddFunctorVector = Coproduct
-- If you want to symbolically add functor vectors that you will
-- eventually give an algebra? that implements the summation

-- Also known as the mu of the monoidal category
{-
densify :: forall a b c. SemiRing c => Kron (Vec a c) (Vec b c) -> Vec (Tuple a b) c 
densify (Kron [(DSum f g): xs]) = \x -> case x of
                                           Tuple a b -> (f a) (g b)
densify (Kron []) = zero
-}

-- by default has appropriate semiring instance
type Vec n c = (n -> c)
type Dual v c = (v -> c)
type D a b = Dual a b
newtype LinOp a b = LinOp (a -> b)
derive instance fLin :: Newtype (LinOp a b) _
-- It's kind of cute that they are all the same thing

instance semiringKron :: (Semiring a, Semiring b) => Semiring (Kron a b) where
   add (Kron x) (Kron y) = Kron $ x <> y
   zero = Kron $ mempty
   mul (Kron x) (Kron y) = Kron $ mul <$> x <*> y  -- ? Can I do this. Or should be matrix multplication? This is remarkablt symmettric with resepct to c (The intended scalar).
   one = Kron [one]

instance matVecKron :: (MatVec f v, MatVec g w) => MatVec (Kron f g) (Kron v w) where
   matvec (Kron a) (Kron x) = Kron $ matvec <$> a <*> x

instance matVecDSum :: (MatVec f v, MatVec g w) => MatVec (DSum f g) (DSum v w) where
   matvec (DSum f g) (DSum v w) = DSum (matvec f v) (matvec g w)

instance semiringDSum :: (Semiring a, Semiring b) => Semiring (DSum a b) where
   add (DSum x y) (DSum a b) = DSum (x + a) (y + b)
   zero = DSum zero zero
   mul (DSum x y) (DSum a b) = DSum (x * a) (y * b)
   one = DSum one one

instance ringDSum :: (Ring a, Ring b) => Ring (DSum a b) where
   sub (DSum x y) (DSum a b) = DSum (x - a) (y - b)

instance divisibleDSum :: (DivisionRing a, DivisionRing b) => DivisionRing (DSum a b) where -- Direct sum of matrices
   recip (DSum a b) = DSum (recip a) (recip b)

dfst (DSum a b) = a  
dsnd (DSum a b) = b 

data Block a b c d = Block a b c d

{-
class Conjugatable a b c d where
   conj :: b -> a -> c -> d   -- produces d = c * a * b
-- conjd :: c -> d -> b -> a

--Conjugatable a b c d, Semiring a, Semiring d => SemiRing b

---Conjugatable a b c d, Semiring a, Semiring d => SemiRing c

-- exits b
data ConjugatePair a b = ConjugatePair (a->b) (b->a)

instance SemiRing (ConjugatePair a b where
   add
   zero = const 0 const 0
   mul f g h l = h , g >>> l >>> h
   const one const one
   -}
{-
instance semiringBlock :: (Semiring a, Conjugatable a b c d, Semiring d) => Semiring (Block a b c d) where
   add (MBlock x y z w) (MBlock a b c d) = MBlock (x+a) (y+b) (z+c) (w+d)
   zero = MBlock zero zero zero zero
   mul (MBlock x y z w) (MBlock a b c d) = MBlock (x*a+y*c) (x*b+y*d) (z*a+w*c) (z*b+w*d)  -- Could use strassen
   one = MBlock one zero zero one
-}


instance semiringBlock :: (Semiring a, Semiring b) => Semiring (Block (LinOp a a) (LinOp b a) (LinOp a b) (LinOp b b)) where
   add (Block x (LinOp y) (LinOp z) w) (Block a (LinOp b) (LinOp c) d) = Block (x+a) (LinOp (y+b)) (LinOp (z+c)) (w+d)
   zero = Block zero (LinOp (const zero)) (LinOp (const zero)) zero
   mul (Block (LinOp x) (LinOp y) (LinOp z) (LinOp w)) (Block (LinOp a) (LinOp b) (LinOp c) (LinOp d)) = Block (LinOp (x<<<a+y<<<c)) (LinOp (x<<<b + y<<<d)) (LinOp (z<<<a+w<<<c)) (LinOp (z<<<b+w<<<d))  -- Could use strassen
   one = Block one (LinOp (const zero)) (LinOp (const zero)) one


{-
--LinOp is not a functor in c.
-- LinOp' is a Profunctor
instance fL :: Functor (LinOp a b) where
map f (LinOp g) = LinOp (map (\x -> map f x) g)
-}

{-
instance aV :: Apply (AVec n) where
apply (AVec f) (AVec x) = AVec (apply f x)

instance a2V :: Sized n => Applicative (AVec n) where
pure x = AVec (replicate (size (Proxy :: Proxy n)) x)
-}

instance semiringLinOp :: Semiring a => Semiring (LinOp a a) where
   add (LinOp f) (LinOp g) = LinOp \x -> (f x) + (g x)
   zero = LinOp (const zero)
   mul (LinOp f) (LinOp g) = LinOp (f <<< g)
   one = LinOp id

ap = unwrap

instance ringLinOp :: Ring a => Ring (LinOp a a) where
   sub (LinOp f) (LinOp g) = LinOp \x -> (f x) - (g x)

leftcol :: forall a b. Semiring b => (LinOp (DSum a b) (DSum a b)) -> a -> (DSum a b)
leftcol (LinOp f) v = f $ DSum v zero

rightcol :: forall a b. Semiring a => (LinOp (DSum a b) (DSum a b)) -> b -> (DSum a b)
rightcol (LinOp f) v = f $ DSum zero v

-- This doesn't help right?
--leftrightcol :: forall a b. Semiring a => (LinOp (DSum a b) (DSum a b)) -> Tuple (a -> (DSum a b)) (b -> (DSum a b))
--leftrightcol (LinOp f) v

instance divisibleLinOp :: (Ring a, Ring b, DivisionRing (LinOp a a), DivisionRing (LinOp b b)) => DivisionRing (LinOp (DSum a b) (DSum a b)) where
recip :: (LinOp (DSum a b) (DSum a b)) -> (LinOp (DSum a b) (DSum a b))
recip (LinOp f) = LinOp \x -> case x of
                                 (DSum v w) -> DSum (m v             +  (m <<< b <<< dinv) (zero-w))
                                                    ((dinv <<< c <<< m) (zero-v)  +  (dinv <<< c <<< m <<< b <<< dinv + dinv) w) where
                                                                  ac = leftcol (LinOp f)
                                                                  a :: a -> a
                                                                  a = dfst <<< ac -- Count on fusion?
                                                                  c :: a -> b
                                                                  c = dsnd <<< ac
                                                                  bd = rightcol (LinOp f)
                                                                  b :: b -> a
                                                                  b = dfst <<< bd
                                                                  d :: b -> b
                                                                  d = dsnd <<< bd
                                                                  dinv :: b -> b
                                                                  dinv = unwrap $ recip (LinOp d)
                                                                  m = unwrap $ recip $ LinOp (a - b <<< dinv <<< c)

-- Every time I compute the b product, I'm also computing the d product. This is very wasteful and dumb.


-- Intended to hold a + (b->b) 
-- to be inverted by sherman morrison formula
data LowRank a b = LowRank a (LinOp b b)

-- a notion of banded need a 4x4 block matrix with just zeros at the corners
-- which means 
--data Banded a b c d e = Banded a b c d e
-- Just some thoughts. This is back into ordinary data type programming.
data Triangular = Blocked Triangular Dense Triangular | Scalar Number
type Dense = Array (Array Number)

--data Triangular a b d = Triangular a b d


instance divisibleLinOpNum :: DivisionRing (LinOp Number Number) where
   recip (LinOp f) = LinOp \x -> x / (f 1.0)

{-
instance semiringLinOp' :: Semiring (AVec a c) => Semiring (AVec a c -> AVec a c) where
add f g = \x -> (f x) + (g x)
zero = (const zero)
mul f g = f <<< g
one = id
-}
--instance matvecLinOp :: MatVec (LinOp a a c) (AVec a c) where
--   matvec (LinOp f) x = f x

dSum :: forall a b c. AVec a c -> AVec b c -> AVec (Either a b) c
dSum (AVec x) (AVec y) = AVec (x <> y)

dSumLeft :: forall a b c. Sized a => AVec (Either a b) c -> AVec a c
dSumLeft (AVec x) = AVec (take (size (Proxy :: Proxy a)) x)
dSumRight :: forall a b c. Sized b => AVec (Either a b) c -> AVec a c
dSumRight (AVec x) = AVec (takeEnd (size (Proxy :: Proxy b)) x)

{-
block :: forall x y c. Sized x => Sized y => LinOp x x c -> LinOp y x c -> LinOp y x c -> LinOp y y c -> LinOp (Either x y) (Either x y) c
block (LinOp a) (LinOp b) (LinOp c) (LinOp d) = LinOp (\x -> let v = dSumLeft x in
let w = dSumRight x in
dSum (a v + b w) (c v + d w))
-}

--unblock :: forall x y c. LinOp (Either x y) (Either x y) c -> Tuple4 (LinOp x x c) (LinOp y x c) (LinOp y x c) (LinOp y y c)

class Transposable f where
   transpose :: f -> f

instance transposeMat :: Transposable a => Transposable (MatF a) where
   transpose (MBlock x y z w) = MBlock (transpose x) (transpose z) (transpose y) (transpose w)

instance transposeNum :: Transposable Number where
   transpose = id

class MatVec a v where
   matvec :: a -> v -> v
-- Converts to LinOp
-- Backconversion via sampling?

class Sampleable a v where
   sample :: (v->v)->a

vfst (VBlock x y) = x
vsnd (VBlock x y) = y
instance sampleMat :: (Sampleable a b, Semiring b) => Sampleable (MatF a) (VecF b) where
   sample f = MBlock a b c d where
                           a = sample (\x -> vfst $ f (VBlock x zero))
                           b = sample (\x -> vsnd $ f (VBlock x zero))
                           c = sample (\x -> vfst $ f (VBlock zero x))
                           d = sample (\x -> vsnd $ f (VBlock zero x))

instance sampleNum :: Sampleable Number Number where
   sample f = f 1.0
-- needs to be typeclassified
{-
hadamardSample :: (VecF a -> VecF a) -> Array (VecF a)
hadamardSample f = [xs] <> [ys] <> zs <> ws where
xs = f (VBlock (one) (zero))
ys = f (VBlock (zero) (one))
zs = sample (\x -> f (VBlock x zero))
ws = sample (\x -> f (VBlock zero x))
-}

{-

hadamard :: Vecf a -> VecF a

-- We are strangely decomposing into evens and odds.
fourier :: VecF a -> VecF a
fourier (VBlock x y) = VBlock (fourier x) (fourier y)
where
twiddle = VBlock twiddle twiddle

-}

-- class definition ~
-- instance definition is like pattern matching on the type.
-- Big Sum type of possible wrappers.

-- Maybe By just having instances of matvec for B and C is easiest?
-- MatVec a v w
-- matvec :: a -> (v -> w)
--matvec :: a -> LinOp v w

instance matVec :: (Semiring v, MatVec f v) => MatVec (MatF f) (VecF v) where
   matvec (MBlock a b c d) (VBlock x y) = VBlock ((matvec a x) + (matvec b y)) ((matvec c x) + (matvec d y))

-- What I'm doing is breaking up a recursive type
-- data Linear = BlockMat | HStack | VStack | Num
-- into a bunch of data constructors
-- and using type class mechanism for recursion
-- I'm going to have to make a new typeclass for every new operation basically.
-- not so great

-- Could use Free or Fix. Not type safe.

{-
instance semiringVec :: Semiring Vec where
add (VNum x) (VNum y) = VNum (x + y)
add (VBlock x y) (VBlock a b) = VBlock (add x a) (add y b)
add _ _ = VNum 0.0

zero = VNum 0.0
mul _ _ = VNum 0.0
one = VNum 0.0

instance semiringVec :: Semiring Mat where
add (MNum x) (MNum y) = VNum (x + y)
add (MBlock a b c d) (MBlock x y z w) = MBlock (add x a) (add y b) (z + c) (w + d)
add _ _ = MNum 0.0

zero = VNum 0.0
mul _ _ = VNum 0.0
one = VNum 0.0

mult :: Mat -> Vec -> Vec
mult (MNum x) (VNum y) = VNum (x * y)
mult (MBlock a b c d) (VBlock x y) = VBlock ((mult a x) + (mult b y)) ((mult c x) + (mult d y))
mult _ _ = VNum 0.0
-}
{-
plus :: Vec -> Vec -> Vec
plus (VNum x) (VNum y) = VNum (x + y)
plus (VBlock x y) (VBlock a b) = VBlock (plus x a) (plus y b)
plus _ _ = VNum 0.0
-}
--inv :: Mat -> Mat




