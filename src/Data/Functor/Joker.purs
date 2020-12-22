module Data.Functor.Joker where

import Prelude

import Control.Biapplicative (class Biapplicative)
import Control.Biapply (class Biapply)

import Data.Bifunctor (class Bifunctor)
import Data.Newtype (class Newtype)

-- | Make a `Functor` over the second argument of a `Bifunctor`
newtype Joker :: forall k1 k2. (k1 -> Type) -> k2 -> k1 -> Type
newtype Joker g a b = Joker (g b)

derive instance newtypeJoker :: Newtype (Joker g a b) _

derive newtype instance eqJoker :: Eq (g b) => Eq (Joker g a b)

derive newtype instance ordJoker :: Ord (g b) => Ord (Joker g a b)

instance showJoker :: Show (g b) => Show (Joker g a b) where
  show (Joker x) = "(Joker " <> show x <> ")"

instance functorJoker :: Functor g => Functor (Joker g a) where
  map g (Joker a) = Joker (map g a)

instance applyJoker :: Apply f => Apply (Joker f a) where
  apply (Joker f) (Joker g) = Joker $ apply f g

instance applicativeJoker :: Applicative f => Applicative (Joker f a) where
  pure = Joker <<< pure

instance bindJoker :: Bind f => Bind (Joker f a) where
  bind (Joker ma) amb = Joker $ ma >>= (amb >>> un Joker)

instance monadJoker :: Monad m => Monad (Joker m a)

instance bifunctorJoker :: Functor g => Bifunctor (Joker g) where
  bimap _ g (Joker a) = Joker (map g a)

instance biapplyJoker :: Apply g => Biapply (Joker g) where
  biapply (Joker fg) (Joker xy) = Joker (fg <*> xy)

instance biapplicativeJoker :: Applicative g => Biapplicative (Joker g) where
  bipure _ b = Joker (pure b)

hoistJoker :: forall f g a b. (f ~> g) -> Joker f a b -> Joker g a b
hoistJoker f (Joker a) = Joker (f a)
