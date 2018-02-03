module Fock where

import Vec

import Data.Lazy

import Data.Functor.Product
import Data.Functor.Compose


data Stream a = Cons' a (Lazy (Stream a))

-- It is like Free with product replacing sum
-- Free f a ~ a + f a + f f a + f f f a + ...
-- Thus has to be lazy 
data FreeTimes f a = FreeTimes a (Lazy (FreeTimes f (f a)))


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
