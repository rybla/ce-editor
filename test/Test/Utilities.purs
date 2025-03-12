module Test.Utilities where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Effect.Exception (Error, error)
import Test.Spec.Assertions (fail)

shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Show t
  => Eq t
  => t
  -> t
  -> m Unit
shouldEqual v1 v2 =
  when (v1 /= v2)
    $ fail
    $ show v1 <> " ≠\n  " <> show v2 <> "\n"

throw = throwError <<< error

