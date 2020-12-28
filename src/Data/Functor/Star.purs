module Data.Functor.Star where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus, empty)

import Data.Distributive (class Distributive, distribute, collect)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Functor.FunctorRight (class FunctorRight)
import Data.Newtype (class Newtype)

-- | `Star` turns a `Functor` into a `Profunctor`.
-- |
-- | `Star f` is also the Kleisli category for `f`
newtype Star :: (Type -> Type) -> Type -> Type -> Type
newtype Star f a b = Star (a -> f b)

derive instance newtypeStar :: Newtype (Star f a b) _

instance semigroupoidStar :: Bind f => Semigroupoid (Star f) where
  compose (Star f) (Star g) = Star \x -> g x >>= f

instance categoryStar :: Monad f => Category (Star f) where
  identity = Star pure

instance functorStar :: Functor f => Functor (Star f a) where
  map f (Star g) = Star (map f <<< g)

instance functorRightStar :: Functor f => FunctorRight (Star f) where
  rmap = map

instance invariantStar :: Invariant f => Invariant (Star f a) where
  imap f g (Star h) = Star (imap f g <<< h)

instance applyStar :: Apply f => Apply (Star f a) where
  apply (Star f) (Star g) = Star \a -> f a <*> g a

instance applicativeStar :: Applicative f => Applicative (Star f a) where
  pure a = Star \_ -> pure a

instance bindStar :: Bind f => Bind (Star f a) where
  bind (Star m) f = Star \x -> m x >>= \a -> case f a of Star g -> g x

instance monadStar :: Monad f => Monad (Star f a)

instance altStar :: Alt f => Alt (Star f a) where
  alt (Star f) (Star g) = Star \a -> f a <|> g a

instance plusStar :: Plus f => Plus (Star f a) where
  empty = Star \_ -> empty

instance alternativeStar :: Alternative f => Alternative (Star f a)

instance monadZeroStar :: MonadZero f => MonadZero (Star f a)

instance monadPlusStar :: MonadPlus f => MonadPlus (Star f a)

instance distributiveStar :: Distributive f => Distributive (Star f a) where
  distribute f = Star \a -> collect (\(Star g) -> g a) f
  collect f = distribute <<< map f

hoistStar :: forall f g a b. (f ~> g) -> Star f a b -> Star g a b
hoistStar f (Star g) = Star (f <<< g)
