module Data.Functor.Product2 where

import Prelude
import Data.Functor.FunctorRight (class FunctorRight, rmap)

-- | The Product of two types that both take two type parameters (e.g. `Either`,
-- | `Tuple, etc.) where both type parameters are the same.
-- |
-- | ```purescript
-- | Product2 (Tuple 4 true) (Right false) :: Product2 Tuple Either Int Boolean
-- | Product2 (Tuple 4 true) (Left      8) :: Product2 Tuple Either Int Boolean
-- | ```
data Product2 :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type
data Product2 f g a b = Product2 (f a b) (g a b)

derive instance eqProduct2 :: (Eq (f a b), Eq (g a b)) => Eq (Product2 f g a b)

derive instance ordProduct2 :: (Ord (f a b), Ord (g a b)) => Ord (Product2 f g a b)

instance showProduct2 :: (Show (f a b), Show (g a b)) => Show (Product2 f g a b) where
  show (Product2 x y) = "(Product2 " <> show x <> " " <> show y <> ")"

instance functorProduct2 :: (Functor (f a), Functor (g a)) => Functor (Product2 f g a) where
  map f (Product2 x y) = Product2 (map f x) (map f y)

instance functorRight :: (FunctorRight f, FunctorRight g) => FunctorRight (Product2 f g) where
  rmap f (Product2 x y) = Product2 (rmap f x) (rmap f y)
