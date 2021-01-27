module Data.Functor.Joker where

import Prelude

import Data.Functor.FunctorRight (class FunctorRight)
import Data.Newtype (class Newtype, un)

-- | This advance type's usage and its relation to `Clown` is best understood
-- | by reading through "Clowns to the Left, Jokers to the Right (Functional
-- | Pearl)"
-- | https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.475.6134&rep=rep1&type=pdf
newtype Joker :: (Type -> Type) -> Type -> Type -> Type
newtype Joker g a b = Joker (g b)

derive instance newtypeJoker :: Newtype (Joker f a b) _

derive newtype instance eqJoker :: Eq (f b) => Eq (Joker f a b)

derive newtype instance ordJoker :: Ord (f b) => Ord (Joker f a b)

instance showJoker :: Show (f b) => Show (Joker f a b) where
  show (Joker x) = "(Joker " <> show x <> ")"

instance functorJoker :: Functor f => Functor (Joker f a) where
  map f (Joker a) = Joker (map f a)

instance functorRightJoker :: Functor f => FunctorRight (Joker f) where
  rmap = map

instance applyJoker :: Apply f => Apply (Joker f a) where
  apply (Joker f) (Joker g) = Joker $ apply f g

instance applicativeJoker :: Applicative f => Applicative (Joker f a) where
  pure = Joker <<< pure

instance bindJoker :: Bind f => Bind (Joker f a) where
  bind (Joker ma) amb = Joker $ ma >>= (amb >>> un Joker)

instance monadJoker :: Monad m => Monad (Joker m a)

hoistJoker :: forall f g a b. (f ~> g) -> Joker f a b -> Joker g a b
hoistJoker f (Joker a) = Joker (f a)
