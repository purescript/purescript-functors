module Data.Functor.Costar where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, (=<=))

import Data.Distributive (class Distributive, distribute)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Functor.FunctorRight (class FunctorRight)
import Data.Newtype (class Newtype)

-- | `Costar` turns a `Functor` into a `Profunctor` "backwards".
-- |
-- | `Costar f` is also the co-Kleisli category for `f`.
newtype Costar :: forall k. (k -> Type) -> k -> Type -> Type
newtype Costar f b a = Costar (f b -> a)

derive instance newtypeCostar :: Newtype (Costar f a b) _

instance semigroupoidCostar :: Extend f => Semigroupoid (Costar f) where
  compose (Costar f) (Costar g) = Costar (f =<= g)

instance categoryCostar :: Comonad f => Category (Costar f) where
  identity = Costar extract

instance functorCostar :: Functor (Costar f a) where
  map f (Costar g) = Costar (f <<< g)

instance functorRightCostar :: FunctorRight (Costar f) where
  rmap = map

instance invariantCostar :: Invariant (Costar f a) where
  imap = imapF

instance applyCostar :: Apply (Costar f a) where
  apply (Costar f) (Costar g) = Costar \a -> f a (g a)

instance applicativeCostar :: Applicative (Costar f a) where
  pure a = Costar \_ -> a

instance bindCostar :: Bind (Costar f a) where
  bind (Costar m) f = Costar \x -> case f (m x) of Costar g -> g x

instance monadCostar :: Monad (Costar f a)

instance distributiveCostar :: Distributive (Costar f a) where
  distribute f = Costar \a -> map (\(Costar g) -> g a) f
  collect f = distribute <<< map f
