module Data.Functor.Joker where

import Prelude

import Data.Functor.FunctorRight (class FunctorRight)
import Data.Newtype (class Newtype, un)

-- | Lifts a type that takes a single type parameter to a type that takes
-- | two type parameters where the first parameter is a phantom type.
-- | If you want the second parameter to be the phantom type instead of
-- | the first type parameter, see the `Clown` newtype.
-- |
-- | ```purescript
-- | data Box a = Box a
-- | -- these values are the same at runtime
-- |        Box a  ::       Box        a
-- | Joker (Box a) :: Joker Box Int    a
-- | Joker (Box a) :: Joker Box String a
-- |
-- | newtype TupleInt a = TupleInt (Tuple Int a)
-- | -- these values are the same at runtime
-- |        TupleInt (Tuple 4 true) ::        TupleInt        Boolean
-- | Joker (TupleInt (Tuple 4 true)) :: Joker TupleInt Int    Boolean
-- | Joker (TupleInt (Tuple 4 true)) :: Joker TupleInt String Boolean
-- | ```
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
