module Test.Expr where

import Data.Expr
import Prelude

import Data.List as List
import Data.Maybe (maybe, maybe')
import Data.Tuple.Nested ((/\))
import Effect.Class.Console as Console
import Test.Spec (Spec, before_, describe, it)
import Test.Spec.Console (tellLns)
import Test.Utilities (shouldEqual, throw)
import Utility (bug)

test :: Spec Unit
test = describe "Expr" do
  test_isAncestorSibling
  test_drag

test_validHandle :: Spec Unit
test_validHandle = describe "validHandle" do
  it "simple" do
    -- invalid Handle: [[ [] | 0 .. 1 | [0] | 0 .. 0 @ OuterRight_HandleFocus ]]
    validHandle (handle [] 0 1 [ 0 ] 0 0 OuterRight_HandleFocus) `shouldEqual` true

test_isAncestorSibling :: Spec Unit
test_isAncestorSibling = describe "isAncestorSibling" do
  it "simple" do
    let h = handle [ 0 ] 2 2 [] 2 2 InnerLeft_HandleFocus
    p_IL <- toPointHandle h # maybe (throw "h should be a Point Handle") pure
    let p_OR = point [] 1
    isAncestorSibling p_OR p_IL `shouldEqual`
      pure (step 0 /\ path [])

test_drag :: Spec Unit
test_drag = describe "drag" do
  it "drag from cursor Right Point to a rightward sibling Point to adjust the cursor Handle" do
    let h = cursor [ 0 ] 0 1 Right_CursorFocus
    let p_R = point [ 0 ] 2
    let h' = cursor [ 0 ] 0 2 Right_CursorFocus
    c <- toCursorHandle h # maybe (throw "h should be a Cursor Handle") pure
    let p_L = getCursorAnchorPoint c
    areOrderedSiblings p_L p_R `shouldEqual` true
    getHandleFromTo h p_R `shouldEqual` pure h'

  it "drag from an inner left Point to an outer Right point to make a selection Handle" do
    let
      h = handle [ 0 ] 2 2 [] 2 2 InnerLeft_HandleFocus
      p_OR = point [] 1
      h' = handle [] 0 1 [ 0 ] 2 2 OuterRight_HandleFocus
    getHandleFromTo h p_OR `shouldEqual` pure h'

index = Index
step = Step
path is = Path (is # List.fromFoldable # map step)
point is j = Point (path is) (index j)
cursor is j_L j_R f = mkCursorHandle $ Cursor (path is) (index j_L) (index j_R) f
handle is_O j_OL j_OR is_I j_IL j_IR f = mkHandle (path is_O) (index j_OL) (index j_OR) (path is_I) (index j_IL) (index j_IR) f

