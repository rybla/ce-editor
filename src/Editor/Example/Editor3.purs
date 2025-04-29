module Editor.Example.Editor3 where

-- import Data.Expr
-- import Prelude

-- import Data.Array as Array
-- import Data.Foldable (fold)
-- import Data.Maybe (fromMaybe)
-- import Data.Newtype (wrap)
-- import Data.String as String
-- import Data.Unfoldable (none)
-- import Editor (Editor(..), mkPasteFragmentEdit)
-- import Editor.Common (assembleExpr_default)
-- import Halogen.HTML as HH

-- data L = String String | Root

-- derive instance Eq L

-- instance Show L where
--   show Root = "#Root"
--   show (String s) = s

-- editor :: Editor L
-- editor = Editor
--   { name: "Editor3"
--   , initial_expr:
--       Root %
--         [ example_expr 2 2
--         , example_expr 2 2
--         , example_expr 2 2
--         ]
--   , initial_handle: Point_Handle (Point { path: mempty, j: wrap 0 })
--   , getEditMenu: \root handle query -> fold
--       [ if String.null query then []
--         else [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ String query % [] ] ]
--       , [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ String "example" % [] ] ]
--       ]
--   , getShortcut: \_ _ _ -> none
--   , isValidHandle: \_ _ -> true
--   , assembleExpr: \args ->
--       case args.label of
--         Root -> Array.fold
--           [ fold $ Array.zipWith (\kid point -> [ point ] <> kid) args.kids args.points
--           , [ args.points # Array.last # fromMaybe (HH.div [] [ HH.text "{{missing last point}}" ]) ]
--           ]
--         _ -> assembleExpr_default args
--   }

-- example_expr :: Int -> Int -> Expr L
-- example_expr _ 0 = String "L" % []
-- example_expr n_branching n_height =
--   String "B" %
--     Array.range 0 (n_branching - 1)
--     <#> \_ -> example_expr n_branching (n_height - 1)

