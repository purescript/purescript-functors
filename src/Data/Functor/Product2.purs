module Data.Functor.Product2 where

import Prelude

-- | The Product of two `Bifunctor`s.
data Product2 :: forall k1 k2. (k1 -> k2 -> Type) -> (k1 -> k2 -> Type) -> k1 -> k2 -> Type
data Product2 f g a b = Product2 (f a b) (g a b)

derive instance eqProduct2 :: (Eq (f a b), Eq (g a b)) => Eq (Product2 f g a b)

derive instance ordProduct2 :: (Ord (f a b), Ord (g a b)) => Ord (Product2 f g a b)

instance showProduct2 :: (Show (f a b), Show (g a b)) => Show (Product2 f g a b) where
  show (Product2 x y) = "(Product2 " <> show x <> " " <> show y <> ")"
