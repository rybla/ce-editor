module Test.Expr where

import Data.ExprNew
import Prelude

import Data.Array as Array
import Data.List as List
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec as Spec
import Test.Utilities (shouldEqual)

test :: Spec Unit
test = describe "Expr" do
  test_drag

test_drag :: Spec Unit
test_drag = describe "drag" do
  Spec.it "drag from a Cursor Handle's Right Point to a rightward sibling Point to adjust the Cursor Handle" $ unsafePartial do
    let h = spanH { path: [ 0 ], j_L: 0, j_R: 1 } Right_SpanFocus
    let p = point [ 0 ] 2
    let h'@(SpanH_Handle sh _) = spanH { path: [ 0 ], j_L: 0, j_R: 2 } Right_SpanFocus
    let hp = sh # getPoints_SpanH
    areOrderedSiblings_Point hp._L p `shouldEqual` true
    drag h p (example_expr 2 2) `shouldEqual` pure h'
    pure unit

  it "drag from an Inner Right Point to an Outer Right Point to make a Select Handle" do
    -- let
    --   h = handle [ 0 ] 2 2 [] 2 2 InnerRight_HandleFocus
    --   p_OR = point [] 1
    --   h' = handle [] 0 1 [ 0 ] 0 2 OuterRight_HandleFocus
    -- getHandleFromTo h p_OR (example_expr 2 2) `shouldEqual` pure h'
    pure unit

point is j = Point { path: path is, j: Index j }
path is = Path (is # List.fromFoldable # map Step)
spanH h f = SpanH_Handle (SpanH { path: path h.path, j_L: Index h.j_L, j_R: Index h.j_R }) f
zipperH h f = ZipperH_Handle (ZipperH { path_O: path h.path_O, j_OL: Index h.j_OL, j_OR: Index h.j_OR, path_I: path h.path_I, j_IL: Index h.j_IL, j_IR: Index h.j_IR }) f

example_expr :: Int -> Int -> Expr
example_expr _ 0 = String "L" % []
example_expr n_branching n_height =
  String "B" %
    Array.range 0 n_branching
    <#> \_ -> example_expr n_branching (n_height - 1)

