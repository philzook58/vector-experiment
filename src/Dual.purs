

-- take the dual of a functor
newtype Dual f a = Dual (f a -> a)


{-
The 1 versus not  versions of typeclasses. Is there really a reason for them?
You want the 1 versions for 
Functor oriented programming

usually
Blank1 f, Blank x => Blank (f x)

The other direction requires higher order contraints

There is also composition.


Recursion occurs in Compose. It is a type level tag that tells you to keep going.

An internal Semiring type
that gets an automatic instance from prelude Semiring 






-}