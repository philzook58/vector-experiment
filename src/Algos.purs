module Algos where

-- Conjugate gradient method

import Vec
import Prelude
-- Iterative algorithms
import Data.List.Lazy

--

{-
conjugategradstep :: ->   (Vec a n -> Vec a n)
where
ap = a p
alpha = dot r z /  (dot ap p)
x' = x + smult alpha p
r' = r - smult alpha ap
z' = m1 
-}


-- Generic iterative inversion

-- Ax = b becomes
-- Px = b + (P-A)x

-- or x = Ainv, b = I 
-- A Ainv = I
-- P Ainv = I + (P-A)Ainv

step a p b x = step' (p-a) (recip p) b x
step' pa pinv b x = matvec pinv $ b + (matvec pa x)

mstep a p ainvn = mstep' (p-a) (recip p) ainvn
mstep' pa pinv ainvn = pinv + pinv * pa * ainvn

-- Useful for seeing these algorithms
data Expr = Lit String | Plus Expr Expr | Times Expr Expr | Inv Expr | Sub Expr Expr | One | Zero

instance showExpr :: Show Expr where
   show (Lit x) = x
   show (Plus x y) = show x <> " + " <> show y
   show (Times x y ) = "(" <> show x <> " * " <> show y <> ")"
   show (Inv x) = "(" <> show x <> ")^-1"
   show (Sub x y) = show x <> " - " <> show y
   show One = "I"
   show Zero = "0" 


instance semiRingExpr :: Semiring Expr where
   add Zero x = x
   add x Zero = x
   add x y = Plus x y
   mul One x = x
   mul x One = x
   mul x y = Times x y
   zero = Zero
   one = One

instance ringExpr :: Ring Expr where
  sub x Zero = x
  sub x y = Sub x y

instance divExpr :: DivisionRing Expr where
   recip x = Inv x 

-- Also. Could use expr to build interpreter that memoizes and optimizes.

-- Can also do Diagrams
-- Times is juxtaposiotion
-- Plus is a literal + picture

powermethodstep a v = a * v 



class GaussSeidel f where
  gsSeries :: forall a. f a -> List (f a) -- DivisionRing a? a is Number only? GaussSeidel a?

-- maybe let
--instance DivisionRing a => GaussSeidel a where
-- gsSeries x = repeat (recip x)

instance GaussSeidel a => GaussSeidel (M2 a) where
 gsSeries (M2 a b c d) = Cons  where
                               s1 = 



instance GaussSeidel a => GaussSeidel (Triangular'' a) where
 gsSeries (Tri' a b c d) = 



