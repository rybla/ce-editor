module Editor.Example.Editor1 where

import Data.Expr
import Prelude

import Data.Array (range)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Editor.Common (Editor)

newtype L meta = L { dat :: Dat, meta :: meta }

derive instance Generic (L meta) _

derive instance Newtype (L meta) _

instance Show (L meta) where
  show (L { dat }) = show dat

instance Eq (L meta) where
  eq (L l1) (L l2) = l1.dat == l2.dat

mkL dat meta = L { dat, meta }

data Dat = Root | String String

derive instance Generic Dat _

instance Show Dat where
  show Root = "#Root"
  show (String s) = s

instance Eq Dat where
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
  , example_fragment: \s -> Just $ Span_Fragment $ Span [ Expr { l: mkL (String s) default_meta, kids: [] } ]
  , max_history_length: 100
  }

example_expr :: forall meta. meta -> Int -> Int -> Expr (L meta)
example_expr default_meta _ 0 = mkL (String "L") default_meta % []
example_expr default_meta n_branching n_height =
  mkL (String "B") default_meta %
    range 0 (n_branching - 1)
    <#> \_ -> example_expr default_meta n_branching (n_height - 1)
