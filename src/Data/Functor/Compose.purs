module Data.Functor.Compose where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus, empty)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse)

-- | `Compose f g` is the composition of the two functors `f` and `g`.
newtype Compose f g a = Compose (f (g a))

bihoistCompose
  :: forall f g h i
   . Functor f
  => (f ~> h)
  -> (g ~> i)
  -> Compose f g
  ~> Compose h i
bihoistCompose natF natG (Compose fga) = Compose (natF (map natG fga))

derive instance newtypeCompose :: Newtype (Compose f g a) _

derive newtype instance eqCompose :: Eq (f (g a)) => Eq (Compose f g a)

derive newtype instance ordCompose :: Ord (f (g a)) => Ord (Compose f g a)

instance showCompose :: Show (f (g a)) => Show (Compose f g a) where
  show (Compose fga) = "(Compose " <> show fga <> ")"

instance functorCompose :: (Functor f, Functor g) => Functor (Compose f g) where
  map f (Compose fga) = Compose $ map f <$> fga

instance applyCompose :: (Apply f, Apply g) => Apply (Compose f g) where
  apply (Compose f) (Compose x) = Compose $ apply <$> f <*> x

instance applicativeCompose :: (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose <<< pure <<< pure

instance foldableCompose :: (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr f i (Compose fga) = foldr (flip (foldr f)) i fga
  foldl f i (Compose fga) = foldl (foldl f) i fga
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance traversableCompose :: (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = map Compose $ traverse (traverse f) fga
  sequence = traverse id

instance altCompose :: (Alt f, Functor g) => Alt (Compose f g) where
  alt (Compose a) (Compose b) = Compose $ alt a b

instance plusCompose :: (Plus f, Functor g) => Plus (Compose f g) where
  empty = Compose empty

instance alternativeCompose :: (Alternative f, Applicative g) => Alternative (Compose f g)
