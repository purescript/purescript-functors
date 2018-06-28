module Test.Main where

import Prelude

import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  assertEqual
    { expected: Compose (Identity (Just true))
    , actual: Compose (Identity (Just true))
    }
  assertEqual
    { expected: Compose (Identity (Just true)) > Compose (Identity (Just false))
    , actual: true
    }
