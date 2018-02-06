module Kron where
import Prelude 
import Vec
import Data.CatList
import Data.Monoid
import Data.Bifunctor
import Data.Foldable (sum)

newtype Kron a b = Kron (CatList (DSum a b))

instance bifunctorKron :: Bifunctor Kron where
   bimap f g (Kron x) = Kron $ map (bimap f g) x 

instance semiringKron :: (Semiring a, Semiring b) => Semiring (Kron a b) where
   add (Kron x) (Kron y) = Kron $ x <> y
   zero = Kron $ mempty
   mul (Kron x) (Kron y) = Kron $ mul <$> x <*> y  -- ? Can I do this. Or should be matrix multplication? This is remarkablt symmettric with resepct to c (The intended scalar).
   one = Kron $ pure one

-- There is probably a MUCH better implementation of this.
-- Also destroys some factorization. Don't use if you don't have to.
lassoc :: forall a b c. Kron a (Kron b c) -> Kron (Kron a b) c
lassoc (Kron x) = Kron $ join $ (flip map) x \a_bc -> case a_bc of 
                                                      DSum a (Kron bcl) -> (flip map) bcl \bc -> case bc of
                                                                                                 DSum b c -> DSum (Kron $ pure (DSum a b)) c


dswap :: forall a b. DSum a b -> DSum b a
dswap (DSum x y) = DSum y x

kswap :: forall a b. Kron a b -> Kron b a 
kswap (Kron x) = Kron $ map dswap x


type KronT f a b = Kron (f a) b
type KronN f b = KronT f Number b
type FreeV b = Kron Number b

{-
type KronLinear a b = a -> (Kron a b)

Piponi's Linear Monad
instance linearMonad :: Semigroup b => Monad (Kron b) where   
   bind x f = 

-}


type KronOp a = Kron (Vec a Number) (Dual (Vec a Number) Number)
{-
instance kronOpmatvec :: MatVec (Kron (Vec a Number) (Dual (Vec a Number) Number)) (Vec a Number) where
   matvec (Kron l) v = sum $ map (\x -> case x of
   								        DSum w dw -> map (\s -> s * (dw v)) w) l

-}