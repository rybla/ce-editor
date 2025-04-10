module Editor.Example.Editor1 where

import Data.Expr
import Prelude

import Data.Array (range)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Editor.Common (Editor)

editor :: Editor
editor =
  { name: "Editor1"
  -- , initial_handle: Cursor_Handle (Cursor (Point none 0) (Point none 0) Left_CursorFocus)
  -- , initial_exprs: []
  -- , initial_exprs: [ example_expr 1 2 ]
  -- , initial_exprs: [ example_expr 3 2 ]
  , initial_exprs: [ example_expr 2 2, example_expr 2 2, example_expr 2 2 ]
  -- , initial_exprs: [ example_expr 1 1 ]
  -- , initial_handle: mkCursorHandle $ Cursor (Path Nil) (Index 0) (Index 0) Left_CursorFocus
  , initial_handle: Point_Handle (Point { path: mempty, j: wrap 0 })
  -- , example_fragment: \s -> Just $ Zipper_Fragment $ Zipper { kids_L: [], kids_R: [], inside: Just $ SpanContext { _O: ExprContext Nil, _I: SpanTooth { l: String s, kids_L: [], kids_R: [] } } }
  , example_fragment: \s -> Just $ Span_Fragment $ Span [ Expr { l: String s, kids: [] } ]
  , max_history_length: 100
  }

example_expr :: Int -> Int -> Expr
example_expr _ 0 = String "L" % []
example_expr n_branching n_height =
  String "B" %
    range 0 (n_branching - 1)
    <#> \_ -> example_expr n_branching (n_height - 1)

