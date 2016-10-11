module Data.Functor.Coproduct where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse, sequence)

-- | `Coproduct f g` is the coproduct of two functors `f` and `g`
newtype Coproduct f g a = Coproduct (Either (f a) (g a))

-- | Left injection
left :: forall f g a. f a -> Coproduct f g a
left fa = Coproduct (Left fa)

-- | Right injection
right :: forall f g a. g a -> Coproduct f g a
right ga = Coproduct (Right ga)

-- | Eliminate a coproduct by providing eliminators for the left and
-- | right components
coproduct :: forall f g a b. (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f _ (Coproduct (Left a)) = f a
coproduct _ g (Coproduct (Right b)) = g b

-- | Change the underlying functors in a coproduct
bihoistCoproduct
  :: forall f g h i
   . (f ~> h)
  -> (g ~> i)
  -> Coproduct f g
  ~> Coproduct h i
bihoistCoproduct natF natG (Coproduct e) = Coproduct (bimap natF natG e)

derive instance newtypeCoproduct :: Newtype (Coproduct f g a) _

derive instance eqCoproduct :: (Eq (f a), Eq (g a)) => Eq (Coproduct f g a)

derive instance ordCoproduct :: (Ord (f a), Ord (g a)) => Ord (Coproduct f g a)

instance showCoproduct :: (Show (f a), Show (g a)) => Show (Coproduct f g a) where
  show (Coproduct (Left fa)) = "(left " <> show fa <> ")"
  show (Coproduct (Right ga)) = "(right " <> show ga <> ")"

instance functorCoproduct :: (Functor f, Functor g) => Functor (Coproduct f g) where
  map f (Coproduct e) = Coproduct (bimap (map f) (map f) e)

instance foldableCoproduct :: (Foldable f, Foldable g) => Foldable (Coproduct f g) where
  foldr f z = coproduct (foldr f z) (foldr f z)
  foldl f z = coproduct (foldl f z) (foldl f z)
  foldMap f = coproduct (foldMap f) (foldMap f)

instance traversableCoproduct :: (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  traverse f = coproduct
    (map (Coproduct <<< Left) <<< traverse f)
    (map (Coproduct <<< Right) <<< traverse f)
  sequence = coproduct
    (map (Coproduct <<< Left) <<< sequence)
    (map (Coproduct <<< Right) <<< sequence)
