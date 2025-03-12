module Test.Expr where

import Data.Expr
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Utilities (shouldEqual)

throw = throwError <<< error

test :: Spec Unit
test = describe "Expr" do
  test_isAncestorSibling
  test_drag

test_isAncestorSibling :: Spec Unit
test_isAncestorSibling = describe "isAncestorSibling" do
  it "simple" do
    let h = mkHandle (p (s 0 : Nil)) (i 2) (i 2) (p Nil) (i 2) (i 2) InnerLeft_HandleFocus
    p_IL <- toPointHandle h # maybe (throw "h should be a Point Handle") pure
    let p_OR = Point (p Nil) (i 1)
    isAncestorSibling p_OR p_IL `shouldEqual`
      pure (s 0 /\ p Nil)

test_drag :: Spec Unit
test_drag = describe "drag" do
  it "drag from an inner left Point to an outer Right point to make a selection Handle" do
    let
      h = mkHandle (p (s 0 : Nil)) (i 2) (i 2) (p Nil) (i 2) (i 2) InnerLeft_HandleFocus
      p_OR = Point (p Nil) (i 1)
      h' = mkHandle (p Nil) (i 0) (i 1) (p (s 0 : Nil)) (i 2) (i 2) OuterRight_HandleFocus
    getHandleFromTo h p_OR `shouldEqual` pure h'

i = Index
s = Step
p = Path
