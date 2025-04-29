module Test.Expr (spec) where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Edit (EditAt)
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Move as Expr.Move
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.NonEmpty (NonEmpty(..))
import Partial.Unsafe (unsafePartial)
import Pretty (pretty)
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Utilities (shouldEqual)
import Utility (impossible)

type L = String

--------------------------------------------------------------------------------

spec :: Spec Unit
spec = Spec.describe "Expr" do
  spec_drag
  spec_move
  spec_edit

spec_drag :: Spec Unit
spec_drag = Spec.describe "drag" do
  Spec.it "drag from a Cursor Handle's Right Point to a rightward sibling Point to adjust the Cursor Handle" $ unsafePartial do
    let
      h = spanH { path: [ 0 ], j_L: 0, j_R: 1 } Right_SpanFocus
      p = point [ 0 ] 2
      h'@(SpanH_Handle sh _) = spanH { path: [ 0 ], j_L: 0, j_R: 2 } Right_SpanFocus
      hp = sh # getEndPoints_SpanH
    shouldEqual show
      (areOrderedSiblings_Point hp._L p)
      true
    shouldEqual show
      (Expr.Drag.drag h p (example_expr 2 2))
      (pure h')

  Spec.it "drag from an Inner Right Point to an Outer Right Point to make a Zipper Handle" do
    let
      h = Point_Handle $ point [ 0 ] 2
      p_OR = point [] 1
      h' = zipperH { path_O: [], j_OL: 0, j_OR: 1, path_I: [ 0 ], j_IL: 0, j_IR: 2 } OuterRight_ZipperFocus
    shouldEqual show
      (Expr.Drag.drag h p_OR (example_expr 2 2))
      (pure h')

  Spec.it "drag from an Inner Point to an Outer Right Point to make a Zipper Handle, where the innermost span is empty" do
    let
      h = Point_Handle $ point [ 0 ] 0
      p_OR = point [] 1
      h' = zipperH { path_O: [], j_OL: 0, j_OR: 1, path_I: [ 0 ], j_IL: 0, j_IR: 0 } OuterRight_ZipperFocus
    shouldEqual show
      (Expr.Drag.drag h p_OR (example_expr 1 1))
      (pure h')

spec_move :: Spec Unit
spec_move = Spec.describe "move" do
  mkTest_movePoint_R (example_expr 2 3)
    (point [] 0)
    (point [ 0 ] 0 # Just)
  mkTest_movePoint_R (example_expr 2 3)
    (point [ 0 ] 0)
    (point [ 0, 0 ] 0 # Just)
  mkTest_movePoint_R (example_expr 2 3)
    (point [ 0, 0 ] 0)
    (point [ 0, 0, 0 ] 0 # Just)
  mkTest_movePoint_R (example_expr 2 3)
    (point [ 0, 0, 0 ] 0)
    (point [ 0, 0 ] 1 # Just)
  pure unit
  where
  mkTest_movePoint_R e p mb_h =
    Spec.it ("movePoint " <> show e <> " " <> show Expr.Move.R <> " " <> show p <> " == " <> show mb_h) do
      shouldEqual show
        (Expr.Move.movePoint e Expr.Move.R p)
        mb_h

spec_edit :: Spec Unit
spec_edit = Spec.describe "edit" do
  mkTest_EditAt "delete zipper at top level"
    { root: ("Root" % [ ("hello" % []), ("Group" % [ ("world" % []) ]) ])
    , handle: ZipperH_Handle (ZipperH { j_IL: (Index 0), j_IR: (Index 1), j_OL: (Index 1), j_OR: (Index 2), path_I: (NonEmpty (Step 1) Nil), path_O: Nil }) OuterLeft_ZipperFocus
    , clipboard: Nothing
    }
    Expr.Edit.delete
    { root: ("Root" % [ ("hello" % []), ("world" % []) ])
    , handle: (SpanH_Handle (SpanH { j_L: (Index 1), j_R: (Index 2), path: Nil }) Left_SpanFocus)
    , clipboard: Nothing
    }

  mkTest_EditAt "delete big zipper at top level"
    { root: ("Root" % [ ("Group" % [ ("Group" % [ ("Group" % [ ("a" % []) ]) ]) ]), ("Group" % [ ("Group" % [ ("Group" % [ ("b" % []) ]) ]) ]) ])
    , handle: (ZipperH_Handle (ZipperH { j_IL: (Index 0), j_IR: (Index 1), j_OL: (Index 1), j_OR: (Index 2), path_I: (NonEmpty (Step 1) ((Step 0) : (Step 0) : Nil)), path_O: Nil }) OuterLeft_ZipperFocus)
    , clipboard: Nothing
    }
    Expr.Edit.delete
    { root: ("Root" % [ ("Group" % [ ("Group" % [ ("Group" % [ ("a" % []) ]) ]) ]), ("b" % []) ])
    , handle: (SpanH_Handle (SpanH { j_L: (Index 1), j_R: (Index 2), path: Nil }) Left_SpanFocus)
    , clipboard: Nothing
    }
  pure unit
  where
  mkTest_Edit
    :: PureEditorState L
    -> Edit L
    -> PureEditorState L
    -> Spec Unit
  mkTest_Edit state edit state' =
    Spec.it ("apply edit " <> pretty edit) do
      let Edit _ result = edit
      shouldEqual show
        (Lazy.force result)
        (state')

  mkTest_EditAt
    :: String
    -> PureEditorState L
    -> EditAt L
    -> PureEditorState L
    -> Spec Unit
  mkTest_EditAt label state editAt state'_expected =
    let
      edit = editAt state.handle state.root
    in
      Spec.it label do
        let state'_actual = applyEdit edit state
        shouldEqual show state'_actual.root state'_expected.root
        shouldEqual show state'_actual.handle state'_expected.handle
        shouldEqual show state'_actual.clipboard state'_expected.clipboard

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

point ∷ Array Int → Int → Point
point is j = Point { path: path is, j: Index j }

path :: Array Int -> Path
path is = is # List.fromFoldable # map Step

nepath :: Array Int -> NePath
nepath is = (is # map Step # List.fromFoldable # toNePath # fromMaybe' (impossible $ "must be non-empty: " <> show is)) :: NePath

spanH ∷ { j_L ∷ Int, j_R ∷ Int, path ∷ Array Int } → SpanFocus → Handle
spanH h f = SpanH_Handle (SpanH { path: path h.path, j_L: Index h.j_L, j_R: Index h.j_R }) f

zipperH ∷ { j_IL ∷ Int, j_IR ∷ Int, j_OL ∷ Int, j_OR ∷ Int, path_I ∷ Array Int, path_O ∷ Array Int } → ZipperFocus → Handle
zipperH h f = ZipperH_Handle (ZipperH { path_O: path h.path_O, j_OL: Index h.j_OL, j_OR: Index h.j_OR, path_I: nepath h.path_I, j_IL: Index h.j_IL, j_IR: Index h.j_IR }) f

example_expr :: Int -> Int -> Expr L
example_expr _ 0 = "L" % []
example_expr n_branching n_height =
  "B" %
    Array.range 0 (n_branching - 1)
    <#> \_ -> example_expr n_branching (n_height - 1)

