module Data.Functor.Product where

import Prelude

import Control.Apply (lift2)

import Data.Bifunctor (bimap)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Tuple (Tuple(..), fst, snd)

-- | `Product f g` is the product of the two functors `f` and `g`.
newtype Product f g a = Product (Tuple (f a) (g a))

-- | Create a product.
product :: forall f g a. f a -> g a -> Product f g a
product fa ga = Product (Tuple fa ga)

bihoistProduct
  :: forall f g h i
   . (f ~> h)
  -> (g ~> i)
  -> Product f g
  ~> Product h i
bihoistProduct natF natG (Product e) = Product (bimap natF natG e)

derive instance newtypeProduct :: Newtype (Product f g a) _

derive instance eqProduct :: (Eq (f a), Eq (g a)) => Eq (Product f g a)

derive instance ordProduct :: (Ord (f a), Ord (g a)) => Ord (Product f g a)

instance showProduct :: (Show (f a), Show (g a)) => Show (Product f g a) where
  show (Product (Tuple fa ga)) = "(product " <> show fa <> " " <> show ga <> ")"

instance functorProduct :: (Functor f, Functor g) => Functor (Product f g) where
  map f (Product fga) = Product (bimap (map f) (map f) fga)

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
  bind (Product (Tuple fa ga)) f =
    product (fa >>= fst <<< unwrap <<< f) (ga >>= snd <<< unwrap <<< f)

instance monadProduct :: (Monad f, Monad g) => Monad (Product f g)
