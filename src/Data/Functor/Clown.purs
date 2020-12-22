module Data.Functor.Clown where

import Prelude

import Data.Newtype (class Newtype)

-- | Make a `Functor` over the first argument of a `Bifunctor`
newtype Clown :: forall k1 k2. (k1 -> Type) -> k1 -> k2 -> Type
newtype Clown f a b = Clown (f a)

derive instance newtypeClown :: Newtype (Clown f a b) _

derive newtype instance eqClown :: Eq (f a) => Eq (Clown f a b)

derive newtype instance ordClown :: Ord (f a) => Ord (Clown f a b)

instance showClown :: Show (f a) => Show (Clown f a b) where
  show (Clown x) = "(Clown " <> show x <> ")"

instance functorClown :: Functor (Clown f a) where
  map _ (Clown a) = Clown a

hoistClown :: forall f g a b. (f ~> g) -> Clown f a b -> Clown g a b
hoistClown f (Clown a) = Clown (f a)
