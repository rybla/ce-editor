module Test.Utilities where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Effect.Exception (Error, error)
import Test.Spec.Assertions (fail)

shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => (t -> String)
  -> t
  -> t
  -> m Unit
shouldEqual toString v1 v2 =
  when (v1 /= v2)
    $ fail
    $ toString v1 <> " ≠\n  " <> toString v2 <> "\n"

throw = throwError <<< error

