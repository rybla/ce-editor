module Test.Expr where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.List as List
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Utilities (shouldEqual)

test :: Spec Unit
test = Spec.describe "Expr" do
  test_drag

test_drag :: Spec Unit
test_drag = Spec.describe "drag" do
  Spec.it "drag from a Cursor Handle's Right Point to a rightward sibling Point to adjust the Cursor Handle" $ unsafePartial do
    let
      h = spanH { path: [ 0 ], j_L: 0, j_R: 1 } Right_SpanFocus
      p = point [ 0 ] 2
      h'@(SpanH_Handle sh _) = spanH { path: [ 0 ], j_L: 0, j_R: 2 } Right_SpanFocus
      hp = sh # getEndPoints_SpanH
    areOrderedSiblings_Point hp._L p `shouldEqual` true
    drag h p (example_expr 2 2) `shouldEqual` pure h'

  Spec.it "drag from an Inner Right Point to an Outer Right Point to make a Zipper Handle" do
    let
      h = Point_Handle $ point [ 0 ] 2
      p_OR = point [] 1
      h' = zipperH { path_O: [], j_OL: 0, j_OR: 1, path_I: [ 0 ], j_IL: 0, j_IR: 2 } OuterRight_ZipperFocus
    drag h p_OR (example_expr 2 2) `shouldEqual` pure h'

  Spec.it "drag from an Inner Point to an Outer Right Point to make a Zipper Handle, where the innermost span is empty" do
    let
      h = Point_Handle $ point [ 0 ] 0
      p_OR = point [] 1
      h' = zipperH { path_O: [], j_OL: 0, j_OR: 1, path_I: [ 0 ], j_IL: 0, j_IR: 0 } OuterRight_ZipperFocus
    drag h p_OR (example_expr 1 1) `shouldEqual` pure h'

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

point is j = Point { path: path is, j: Index j }
path is = Path (is # List.fromFoldable # map Step)
spanH h f = SpanH_Handle (SpanH { path: path h.path, j_L: Index h.j_L, j_R: Index h.j_R }) f
zipperH h f = ZipperH_Handle (ZipperH { path_O: path h.path_O, j_OL: Index h.j_OL, j_OR: Index h.j_OR, path_I: path h.path_I, j_IL: Index h.j_IL, j_IR: Index h.j_IR }) f

example_expr :: Int -> Int -> Expr
example_expr _ 0 = String "L" % []
example_expr n_branching n_height =
  String "B" %
    Array.range 0 (n_branching - 1)
    <#> \_ -> example_expr n_branching (n_height - 1)

