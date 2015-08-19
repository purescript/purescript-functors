-- | `Functor`/`Monad` products

module Data.Functor.Product
  ( Product(..)
  , runProduct
  , product
  ) where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Traversable (Traversable, traverse, sequence)
import Data.Bifunctor (bimap)

import Control.Apply (lift2)

-- | `Product f g` is the product of the two functors `f` and `g`.
newtype Product f g a = Product (Tuple (f a) (g a))

-- | Unwrap a product
runProduct :: forall f g a. Product f g a -> Tuple (f a) (g a)
runProduct (Product p) = p

-- | Create a product.
product :: forall f g a. f a -> g a -> Product f g a
product fa ga = Product (Tuple fa ga)

instance functorProduct :: (Functor f, Functor g) => Functor (Product f g) where
  map f = Product <<< bimap (map f) (map f) <<< runProduct

instance foldableProduct :: (Foldable f, Foldable g) => Foldable (Product f g) where
  foldr f z (Product (Tuple fa ga)) = foldr f (foldr f z ga) fa
  foldl f z (Product (Tuple fa ga)) = foldl f (foldl f z fa) ga
  foldMap f (Product (Tuple fa ga)) = foldMap f fa <> foldMap f ga

instance traversableProduct :: (Traversable f, Traversable g) => Traversable (Product f g) where
  traverse f (Product (Tuple fa ga)) = lift2 product (traverse f fa) (traverse f ga)
  sequence (Product (Tuple fa ga)) = lift2 product (sequence fa) (sequence ga)
  
instance applyProduct :: (Apply f, Apply g) => Apply (Product f g) where
  apply (Product (Tuple f g)) (Product (Tuple a b)) = product (apply f a) (apply g b)
  
instance applicativeProduct :: (Applicative f, Applicative g) => Applicative (Product f g) where
  pure a = product (pure a) (pure a)
 
instance bindProduct :: (Bind f, Bind g) => Bind (Product f g) where
  bind (Product (Tuple fa ga)) f = product (fa >>= fst <<< runProduct <<< f) 
                                           (ga >>= snd <<< runProduct <<< f)

instance monadProduct :: (Monad f, Monad g) => Monad (Product f g)