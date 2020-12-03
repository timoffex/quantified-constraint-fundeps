{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}

-- | Workaround for the fact that quantified constraints ignore
-- fundeps.
--
-- This compiles with GHC 8.6.3 (and the "naive implementation"
-- doesn't work in this version).
module QuantifiedConstraintFundeps where


-- | Some class with fundeps.
--
-- This class allows creating a value of type @a@ from a value of type
-- @b@, such that @a@ determines @b@.
class C a b | a -> b where
  make :: b -> a


-- | A data type that is usually quantified over @s@.
--
-- Suggestively named ST. In fact, this entire example is a
-- stripped-down version of something I got while experimenting with
-- creating an effect in fused-effects that uses a quantified type
-- parameter to enforce a contract, in the style of ST! In my case, C
-- is the Algebra typeclass, and the sig in my quantified constraint
-- couldn't be inferred.
data ST s where
  ST :: ST s


-- | The function we would like to implement.
--
-- This takes in a function that turns @forall s. st s@ into a @y@.
-- The constraint allows creating a value of type @st s@ from a value
-- of type @ST s@ for every @s@.
fun :: (forall s. C (st s) (ST s))
    => ((forall s. st s) -> y)
    -> y

{- The naive implementation doesn't work.

fun extract = extract $ make ST

    • Could not deduce (C (st s) (ST s0)) arising from a use of ‘make’
      from the context: forall (s :: k). C (st s) (ST s)
        bound by the type signature for:
                   fun :: forall k (st :: k -> *) y.
                          (forall (s :: k). C (st s) (ST s)) =>
                          ((forall (s :: k). st s) -> y) -> y
        at QuantifiedConstraintFundeps.hs:(39,1)-(41,8)
      The type variable ‘s0’ is ambiguous
      Relevant bindings include
        extract :: (forall (s :: k). st s) -> y
          (bound at QuantifiedConstraintFundeps.hs:44:5)
        fun :: ((forall (s :: k). st s) -> y) -> y
          (bound at QuantifiedConstraintFundeps.hs:44:1)
    • In the second argument of ‘($)’, namely ‘make ST’
      In the expression: extract $ make ST
      In an equation for ‘fun’: fun extract = extract $ make ST
-}

-- Solution: instantiate the quantified constraint separately from
-- solving the fundeps.
--
-- @helper@ uses a specialized constraint (not quantified), so fundeps
-- work when it is compiled. Using @helper@ as an argument to
-- @extract@ is a matter of specializing @forall s. C (st s) (ST s)@
-- to @C (st s0) (ST s0)@, which -XQuantifiedConstraints can do no
-- problem.
fun extract = extract helper
  where
    helper :: (C (st s) (ST s)) => st s
    helper = make ST
