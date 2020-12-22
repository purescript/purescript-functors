module Data.Functor.FunctorRight where

-- | Same as `Functor` but works on types that take two type parameters
-- | instead of just one.
class FunctorRight :: forall k. (k -> Type -> Type) -> Constraint
class FunctorRight f where
  rmap :: forall a b c. (b -> c) -> f a b -> f a c
