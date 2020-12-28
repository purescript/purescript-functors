module Data.Functor.Wrap where

import Prelude

import Data.Functor.FunctorRight (class FunctorRight, rmap)
import Data.Newtype (class Newtype)

-- | Given a type that takes two type parameters, `p`, provides a `Functor`
-- | over the second type parameter, `b` as opposed to the first type
-- | parameter, `a`.
newtype Wrap :: forall k. (k -> Type -> Type) -> k -> Type -> Type
newtype Wrap p a b = Wrap (p a b)

derive instance newtypeWrap :: Newtype (Wrap p a b) _

derive newtype instance eqWrap :: Eq (p a b) => Eq (Wrap p a b)

derive newtype instance ordWrap :: Ord (p a b) => Ord (Wrap p a b)

instance showWrap :: Show (p a b) => Show (Wrap p a b) where
  show (Wrap x) = "(Wrap " <> show x <> ")"

instance functorWrap :: FunctorRight p => Functor (Wrap p a) where
  map f (Wrap a) = Wrap (rmap f a)

instance functorRightWrap :: FunctorRight p => FunctorRight (Wrap p) where
  rmap = map
