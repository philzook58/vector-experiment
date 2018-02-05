module Fock where

import Control.Comonad.Cofree
import Data.Functor.Compose
import Data.Functor.Product
import Data.Lazy
import Vec

import Control.Comonad.Traced (listen)


data Stream a = Cons' a (Lazy (Stream a))

-- It is like Free with product replacing sum
-- Free f a ~ a + f a + f f a + f f f a + ...
-- Thus has to be lazy 
data FreeTimes f a = FreeTimes a (Lazy (FreeTimes f (f a)))
-- By analogy with Free, is there a CPS version?
-- Is this in some sense the dual of free? 
-- motherfucker. This is the cofree comonad.

type Fock' v = Cofree v Number


-- The Product type of two vectors is their directsum. The kron is their functor composition
-- hence the fock space is given by 1 d+ v d+ vv d+ vvv +...
-- which translates to in haskellese
-- (Number, v Number, v v Number, ...)
type Fock v = FreeTimes v Number 

data Box

-- dense functor vector
newtype Psi a = Psi (Vec Box a)

-- Kronized functor vector
newtype Psi' a = Psi' (Kron Psi'' a)
-- Pure single particle
type Psi'' = Vec Box Number

newtype BoxFock = BoxFock (Fock Psi)


type BoxFockOp = LinOp BoxFock BoxFock 
-- That's one parenthesization 
-- Does that last thing have to be LinOp Number Number? Or can it just be Number?
type BoxFockOp' = FreeTimes (Kron (LinOp (Psi Number) (Psi Number))) (LinOp Number Number) 


--type DerivTower a = a -> (FreeTimes (Kron (LinOp (Vec a Number))) Number)



-- There is a natural product 
-- FreeTimes is very analogous to a power series
-- There is a natural notion of multiplication
-- COnsider the Fock Space of multiple particle types.
-- (a, fa,ga, f g a, ffa, gga, ... )
--type FreeComp f g a = f g a
-- It is just functor composition.
-- I guess power series "addition" would be Functor Product?

type PowProd = Compose
type PowSum = Product


--what is i gave vectors phantom parameters so that they had an ordering, and then travserse game fermionic
-- minus signs?

-- equating somehow f g and g f ... symmettric particles
-- BosonProd f g ~ (f g , g f)


-- The free monad has an interesting notion of sum
-- You of course can just sum the resulting functor
-- but what about 
-- interleaving and collecting.
-- 1 + (f+g) (ff+gg) + (fff + ggg) + ...
-- (1 + f + ff + ..)+(1 + g + gg + ...) -> (interleaved)
-- There are two sums? One at Functor level, the other at value level?
-- data FreeHelper ff f g gg a = ((ff + gg) a) (FreeHelper (compose f ff)    a | Pure?
--- maybe I'm not making any sense.




-- functiony list
-- occurred during investiagtion into multivariable power series
-- basically CoFree with a representably functor
-- Make it Lazy btw
-- but also Identical with the Consumer coroutine.
data FunctionyStream a b = Consy b (a -> (FunctionyStream a b)) 











