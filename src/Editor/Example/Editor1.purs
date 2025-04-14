module Editor.Example.Editor1 where

import Data.Expr
import Prelude

import Data.Array (range)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Editor.Common (Editor)

data L meta = L L' meta

derive instance Generic (L meta) _

instance Show (L meta) where
  show (L l _) = show l

instance Eq (L meta) where
  eq (L l1 _) (L l2 _) = l1 == l2

data L' = Root | String String

derive instance Generic L' _

instance Show L' where
  show Root = "#Root"
  show (String s) = s

instance Eq L' where
  eq x = genericEq x

mkEditor :: forall meta. meta -> Editor (L meta)
mkEditor default_meta =
  { name: "Editor1"
  -- , initial_handle: Cursor_Handle (Cursor (Point none 0) (Point none 0) Left_CursorFocus)
  -- , initial_exprs: []
  -- , initial_exprs: [ example_expr 1 2 ]
  -- , initial_exprs: [ example_expr 3 2 ]
  , initial_exprs:
      [ example_expr default_meta 2 2
      , example_expr default_meta 2 2
      , example_expr default_meta 2 2
      ]
  -- , initial_exprs: [ example_expr 1 1 ]
  -- , initial_handle: mkCursorHandle $ Cursor (Path Nil) (Index 0) (Index 0) Left_CursorFocus
  , initial_handle: Point_Handle (Point { path: mempty, j: wrap 0 })
  -- , example_fragment: \s -> Just $ Zipper_Fragment $ Zipper { kids_L: [], kids_R: [], inside: Just $ SpanContext { _O: ExprContext Nil, _I: SpanTooth { l: String s, kids_L: [], kids_R: [] } } }
  , example_fragment: \s -> Just $ Span_Fragment $ Span [ Expr { l: L (String s) default_meta, kids: [] } ]
  , max_history_length: 100
  }

example_expr :: forall meta. meta -> Int -> Int -> Expr (L meta)
example_expr default_meta _ 0 = L (String "L") default_meta % []
example_expr default_meta n_branching n_height =
  L (String "B") default_meta %
    range 0 (n_branching - 1)
    <#> \_ -> example_expr default_meta n_branching (n_height - 1)

