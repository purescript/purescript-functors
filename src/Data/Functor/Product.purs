-- | Functor products

module Data.Functor.Product
  ( Product(..)
  , runProduct
  ) where

import Prelude

import Data.Tuple
import Data.Foldable
import Data.Traversable
import Data.Bifunctor (bimap)

import Control.Apply (lift2)

-- | `Product f g` is the product of the two functors `f` and `g`.
newtype Product f g a = Product (Tuple (f a) (g a))

-- | Unwrap a product
runProduct :: forall f g a. Product f g a -> Tuple (f a) (g a)
runProduct (Product p) = p

instance functorProduct :: (Functor f, Functor g) => Functor (Product f g) where
  map f = Product <<< bimap (map f) (map f) <<< runProduct

instance foldableProduct :: (Foldable f, Foldable g) => Foldable (Product f g) where
  foldr f z (Product (Tuple fa ga)) = foldr f (foldr f z ga) fa
  foldl f z (Product (Tuple fa ga)) = foldl f (foldl f z fa) ga
  foldMap f (Product (Tuple fa ga)) = foldMap f fa <> foldMap f ga

instance traversableProduct :: (Traversable f, Traversable g) => Traversable (Product f g) where
  traverse f (Product (Tuple fa ga)) = map Product (lift2 Tuple (traverse f fa) (traverse f ga))
  sequence (Product (Tuple fa ga)) = map Product (lift2 Tuple (sequence fa) (sequence ga))