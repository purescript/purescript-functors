module Data.Functor.Compose where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus, empty)
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Functor.App (hoistLiftApp)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple, curry)

-- | `Compose f g` is the composition of the two functors `f` and `g`.
newtype Compose ∷ ∀ k1 k2. (k2 → Type) → (k1 → k2) → k1 → Type
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

instance eqCompose :: (Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a) where
  eq (Compose fga1) (Compose fga2) =
    eq1 (hoistLiftApp fga1) (hoistLiftApp fga2)

derive instance eq1Compose :: (Eq1 f, Eq1 g) => Eq1 (Compose f g)

instance ordCompose :: (Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a) where
  compare (Compose fga1) (Compose fga2) =
    compare1 (hoistLiftApp fga1) (hoistLiftApp fga2)

derive instance ord1Compose :: (Ord1 f, Ord1 g) => Ord1 (Compose f g)

instance showCompose :: Show (f (g a)) => Show (Compose f g a) where
  show (Compose fga) = "(Compose " <> show fga <> ")"

instance functorCompose :: (Functor f, Functor g) => Functor (Compose f g) where
  map f (Compose fga) = Compose $ map f <$> fga

instance functorWithIndexCompose :: (FunctorWithIndex a f, FunctorWithIndex b g) => FunctorWithIndex (Tuple a b) (Compose f g) where
  mapWithIndex f (Compose fga) = Compose $ mapWithIndex (mapWithIndex <<< curry f) fga

instance applyCompose :: (Apply f, Apply g) => Apply (Compose f g) where
  apply (Compose f) (Compose x) = Compose $ apply <$> f <*> x

instance applicativeCompose :: (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose <<< pure <<< pure

instance foldableCompose :: (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr f i (Compose fga) = foldr (flip (foldr f)) i fga
  foldl f i (Compose fga) = foldl (foldl f) i fga
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance foldableWithIndexCompose :: (FoldableWithIndex a f, FoldableWithIndex b g) => FoldableWithIndex (Tuple a b) (Compose f g) where
  foldrWithIndex f i (Compose fga) = foldrWithIndex (\a -> flip (foldrWithIndex (curry f a))) i fga
  foldlWithIndex f i (Compose fga) = foldlWithIndex (foldlWithIndex <<< curry f) i fga
  foldMapWithIndex f (Compose fga) = foldMapWithIndex (foldMapWithIndex <<< curry f) fga

instance traversableCompose :: (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = map Compose $ traverse (traverse f) fga
  sequence = traverse identity

instance traversableWithIndexCompose :: (TraversableWithIndex a f, TraversableWithIndex b g) => TraversableWithIndex (Tuple a b) (Compose f g) where
  traverseWithIndex f (Compose fga) = map Compose $ traverseWithIndex (traverseWithIndex <<< curry f) fga

instance altCompose :: (Alt f, Functor g) => Alt (Compose f g) where
  alt (Compose a) (Compose b) = Compose $ alt a b

instance plusCompose :: (Plus f, Functor g) => Plus (Compose f g) where
  empty = Compose empty

instance alternativeCompose :: (Alternative f, Applicative g) => Alternative (Compose f g)
