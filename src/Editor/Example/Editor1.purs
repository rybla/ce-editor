module Editor.Example.Editor1 where

import Data.Expr
import Prelude

import Data.Array (range)
import Data.Unfoldable (none)
import Editor.Common (Editor)

editor :: Editor
editor =
  { name: "Editor1"
  , initial_exprs: [ example_expr 2 2 ]
  , initial_handle: Cursor (Index none 0) (Index none 0)
  }

example_expr :: Int -> Int -> Expr
example_expr _ 0 = String "L" % []
example_expr n_branching n_height =
  String "B" %
    range 0 n_branching
    <#> \_ -> example_expr n_branching (n_height - 1)

