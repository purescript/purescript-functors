module Data.Functor.Clown where

import Prelude

import Data.Functor.FunctorRight (class FunctorRight)
import Data.Newtype (class Newtype)

-- | This advance type's usage and its relation to `Joker` is best understood
-- | by reading through "Clowns to the Left, Jokers to the Right (Functional
-- | Pearl)"
-- | https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.475.6134&rep=rep1&type=pdf
newtype Clown :: (Type -> Type) -> Type -> Type -> Type
newtype Clown f a b = Clown (f a)

derive instance newtypeClown :: Newtype (Clown f a b) _

derive newtype instance eqClown :: Eq (f a) => Eq (Clown f a b)

derive newtype instance ordClown :: Ord (f a) => Ord (Clown f a b)

instance showClown :: Show (f a) => Show (Clown f a b) where
  show (Clown x) = "(Clown " <> show x <> ")"

instance functorClown :: Functor (Clown f a) where
  map _ (Clown a) = Clown a

instance functorRightClown :: FunctorRight (Clown f) where
  rmap = map

hoistClown :: forall f g a b. (f ~> g) -> Clown f a b -> Clown g a b
hoistClown f (Clown a) = Clown (f a)
