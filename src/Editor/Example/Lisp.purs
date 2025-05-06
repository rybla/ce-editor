module Editor.Example.Lisp where

-- import Prelude

-- import Control.Plus (empty)
-- import Data.Array as Array
-- import Data.Either (Either(..))
-- import Data.Expr (Expr(..), ExprContext(..), Fragment(..), Handle(..), Point(..), Span(..), SpanContext(..), SpanTooth(..), Tooth(..), Zipper(..), atSubExpr, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth)
-- import Data.Expr.Edit as Expr.Edit
-- import Data.Foldable (and, fold, length)
-- import Data.List (List(..), (:))
-- import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Newtype (wrap)
-- import Data.Set as Set
-- import Data.String as String
-- import Data.TraversableWithIndex (traverseWithIndex)
-- import Data.Tuple.Nested ((/\))
-- import Data.Unfoldable (none)
-- import Editor.Common (Editor(..), renderWarning)
-- import Editor.Notation as Notation
-- import Halogen.HTML as HH
-- import Options.Applicative.Internal.Utils (startsWith)
-- import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
-- import Ui.Halogen (classes)
-- import Utility (isWhitespaceFree)

-- newtype C = C String

-- instance Show C where
--   show (C s) = s

-- derive newtype instance Eq C

-- derive newtype instance Ord C

-- mkExprC c es = mkExpr { con: c } es

-- infix 0 mkExprC as %

-- mkToothC c es = mkTooth { con: c } es

-- infix 0 mkToothC as %<

-- mkSpanToothC c es = mkSpanTooth { con: c } es

-- infix 0 mkSpanToothC as %<*

-- editor :: Editor C
-- editor = Editor
--   { name: "Lisp"
--   , initialExpr: "Root" % []
--   , initialHandle: Point_Handle (Point { path: mempty, j: wrap 0 })
--   , getEditMenu: \state query -> fold
--       [ case query of
--           "" -> []
--           _ | "group" # startsWith (String.Pattern query) -> case state.mb_handle of
--             Just (Point_Handle _) ->
--               [ "literal" /\ pasteLiteral query state
--               , "group" /\ pasteGroup_Span state
--               ]
--             Just (SpanH_Handle _ _) ->
--               [ "literal" /\ pasteLiteral query state
--               , "group" /\ pasteGroup_Zipper state
--               ]
--             Just (ZipperH_Handle _ _) ->
--               [ "group" /\ pasteGroup_Zipper state
--               ]
--             _ -> none
--           _ | "integral" # startsWith (String.Pattern query) -> case state.mb_handle of
--             Just (Point_Handle _) ->
--               [ "literal" /\ pasteLiteral query state
--               , "integral" /\ pasteIntegral_Zipper state
--               ]
--             Just (SpanH_Handle _ _) ->
--               [ "literal" /\ pasteLiteral query state
--               , "integral" /\ pasteIntegral_Zipper state
--               ]
--             Just (ZipperH_Handle _ _) ->
--               [ "integral" /\ pasteIntegral_Zipper state
--               ]
--             _ -> none
--           _ | "linebreak" # startsWith (String.Pattern query) -> case state.mb_handle of
--             Just (Point_Handle _) ->
--               [ "literal" /\ pasteLiteral query state
--               , "linebreak" /\ pasteLineBreak_Span state
--               ]
--             Just (SpanH_Handle _ _) ->
--               [ "literal" /\ pasteLiteral query state
--               , "linebreak" /\ pasteLineBreak_Span state
--               ]
--             Just (ZipperH_Handle _ _) ->
--               none
--             _ -> none
--           _ | query # isWhitespaceFree -> case state.mb_handle of
--             Just (Point_Handle _) -> [ "literal" /\ pasteLiteral query state ]
--             Just (SpanH_Handle _ _) -> [ "literal" /\ pasteLiteral query state ]
--             Just (ZipperH_Handle _ _) -> none
--             _ -> none
--           _ ->
--             none
--       ]
--   , getShortcut: \ki state -> case unit of
--       _ | ki # matchKeyInfoPattern' [ keyEq "(", not_cmd, not_alt ] -> do
--         pasteGroup_Zipper state
--       _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] -> do
--         pasteLineBreak_Span state
--       _ -> empty
--   , isValidHandle: \expr handle -> case handle of
--       Point_Handle p -> and [ isValidPoint expr p ]
--       SpanH_Handle sh _ -> and [ isValidPoint expr p._L, isValidPoint expr p._R ]
--         where
--         p = getEndPoints_SpanH sh
--       ZipperH_Handle zh _ -> and [ isValidPoint expr p._OL, isValidPoint expr p._IL, isValidPoint expr p._IR, isValidPoint expr p._OR ]
--         where
--         p = getEndPoints_ZipperH zh
--   , assembleExpr:
--       let
--         root = Notation.parseString "*"
--         group = Notation.parseString "'(' * ')'"
--         integral = Notation.parseString "'(' ∫ _ from _ to _ of _ ')'"
--         linebreak = Notation.parseString "\n"
--       in
--         Notation.mkAssembleExpr case _ of
--           { label: "Root" } -> Left root
--           { label: "Group" } -> Left group
--           { label: "LineBreak" } -> Left linebreak
--           { label: "Integral" } -> Left integral
--           args@{ label: "Arg" }
--             | length args.kids == 0 -> Right $ pure $
--                 [ HH.div [ classes [ "Token", "before-empty-Arg-kids" ] ] []
--                 , args.points # Array.last # fromMaybe (renderWarning $ "missing point #" <> show @Int (length args.points))
--                 ]
--             | otherwise -> Right do
--                 kids <- args.kids # traverseWithIndex \i -> map (i /\ _)
--                 pure $ fold $
--                   [ fold $ Array.zipWith
--                       ( \point (i /\ kid) -> fold
--                           [ [ point ]
--                           , if i == 0 then [] else [ HH.div [ classes [ "Token", "punctuation", "before-extra-Arg-kid" ] ] [ HH.text "[" ] ]
--                           , kid
--                           , if i == 0 then [] else [ HH.div [ classes [ "Token", "punctuation", "after-extra-Arg-kid" ] ] [ HH.text "]" ] ]
--                           ]
--                       )
--                       args.points
--                       kids
--                   , [ args.points # Array.last # fromMaybe do renderWarning $ "missing point #" <> show @Int (length args.points) ]
--                   ]
--           { label } -> Left [ Notation.Punc do pure [ HH.div [ classes [ "Token", "punctuation", "keyword" ] ] [ HH.text label ] ] ]
--   , printExpr: const "unimplemented"
--   }

-- isValidPoint :: Expr C -> Point -> Boolean
-- isValidPoint expr (Point p) = e'.l `Set.member` ls
--   where
--   Expr e' = (expr # atSubExpr p.path).here
--   ls = Set.fromFoldable [ C "Root", C "Group", C "Arg" ]

-- pasteLiteral query = Expr.Edit.insert $ Span_Fragment $ Span [ query % [] ]

-- pasteGroup_Span = Expr.Edit.insert $ Span_Fragment $ Span [ "Group" % [] ]
-- pasteGroup_Zipper = Expr.Edit.insert $ Zipper_Fragment $ Zipper
--   { kids_L: []
--   , inside: SpanContext
--       { _O: ExprContext Nil
--       , _I: SpanTooth { l: "Group", kids_L: [], kids_R: [] }
--       }
--   , kids_R: []
--   }

-- pasteIntegral_Span = Expr.Edit.insert $ Span_Fragment $ Span [ "Integral" % [ "Arg" % [], "Arg" % [], "Arg" % [], "Arg" % [] ] ]

-- pasteIntegral_Zipper = Expr.Edit.insert $ Zipper_Fragment $ Zipper
--   { kids_L: []
--   , inside: SpanContext
--       { _O: ExprContext $ Tooth { l: "Integral", kids_L: [ "Arg" % [], "Arg" % [], "Arg" % [] ], kids_R: [] } : Nil
--       , _I: SpanTooth { l: "Arg", kids_L: [], kids_R: [] }
--       }
--   , kids_R: []
--   }

-- pasteLineBreak_Span = Expr.Edit.insert $ Span_Fragment $ Span [ "LineBreak" % [] ]

