{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Solutions to problems in https://gitlab.haskell.org/ghc/ghc/-/issues/15351
module SimplestExample where

class C a b | a -> b where
  foo :: a -> b


bar :: (forall x. C (f x) Int) => f a -> String
bar = show . helper
  where
    helper :: C (f x) Int => f x -> Int
    helper = foo





class M a (m :: * -> *) | m -> a
class T a (t :: (* -> *) -> * -> *) | t -> a
--instance (forall m. (M a (t m))) => T a t
--instance (M a (t Maybe)) => T a t
instance ( forall m. M a (t m)
         , M a (t Maybe)) => T a t
