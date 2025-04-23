module Editor.Example.Editor2 where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.Foldable (fold, null)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as String
import Editor (Editor)

data L = String String | Root

derive instance Eq L

instance Show L where
  show Root = "#Root"
  show (String s) = s

editor :: Editor L
editor =
  { name: "Editor2"
  , initial_expr:
      Root %
        [ example_expr 2 2
        , example_expr 2 2
        , example_expr 2 2
        ]
  , initial_handle: Point_Handle (Point { path: mempty, j: wrap 0 })
  , bufferOptions: \h e q -> fold
      [ if String.null q then []
        else [ PasteSpan_BufferOption q $ Span [ String q % [] ] ]
      , [ PasteSpan_BufferOption "example" $ Span [ String "example" % [] ] ]
      ]
  , max_history_length: 100
  }

example_expr :: Int -> Int -> Expr L
example_expr _ 0 = String "L" % []
example_expr n_branching n_height =
  String "B" %
    Array.range 0 (n_branching - 1)
    <#> \_ -> example_expr n_branching (n_height - 1)

