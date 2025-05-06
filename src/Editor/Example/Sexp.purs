module Editor.Example.Sexp where

import Prelude

import Control.Monad.Reader (ask, local)
import Control.Plus (empty)
import Data.Array as Array
import Data.Expr (Expr(..), Fragment(..), Handle(..), Index(..), Point(..), Span(..), atPoint, atSubExpr, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth)
import Data.Expr.Edit as Expr.Edit
import Data.Foldable (and, fold)
import Data.List (List(..))
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe)
import Editor (Label(..))
import Editor.Common (Editor(..), assembleExpr_default, getCon)
import Editor.Notation (literal, punctuation)
import Halogen.HTML as HH
import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)

newtype C = C String

instance Show C where
  show (C s) = s

derive newtype instance Eq C

derive newtype instance Ord C

mkExprC c es = mkExpr (Label { con: c }) es

infix 0 mkExprC as %

mkToothC c es = mkTooth (Label { con: c }) es

infix 0 mkToothC as %<

mkSpanToothC c es = mkSpanTooth (Label { con: c }) es

infix 0 mkSpanToothC as %<*

editor :: Editor C
editor = Editor
  { name: "Sexp"
  , initialExpr: C "Root" % []
  , initialHandle: Point_Handle $ Point { path: mempty, j: wrap 0 }
  , getEditMenu: \state query -> case query of
      "group" ->
        [ "Symbol" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_Symbol query ])) state
        , "Group" /\ Expr.Edit.insert (Zipper_Fragment zipper_Group) state
        ]
      "linebreak" ->
        [ "LineBreak" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
        ]
      _ ->
        [ "Symbol" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_Symbol query ])) state
        ]
  , getShortcut: \ki state -> case unit of
      _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] -> do
        Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
      _ | ki # matchKeyInfoPattern' [ keyEq "(", not_cmd, not_alt ] -> do
        Expr.Edit.insert (Zipper_Fragment zipper_Group) state
      _ -> empty
  , isValidHandle: \root handle -> case handle of
      Point_Handle p -> and [ isValidPoint root p ]
      SpanH_Handle sh _ -> and [ isValidPoint root p._L, isValidPoint root p._R ]
        where
        p = getEndPoints_SpanH sh
      ZipperH_Handle zh _ -> and [ isValidPoint root p._OL, isValidPoint root p._IL, isValidPoint root p._IR, isValidPoint root p._OR ]
        where
        p = getEndPoints_ZipperH zh
  , assembleExpr: \args -> do
      ctx <- ask
      case (args.label # getCon) /\ args.points /\ args.kids of
        C "Root" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ Array.zipWith (\p k -> [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ]
        -- 
        C "Symbol" /\ [ _p0, _p1 ] /\ [ k0 ] -> do
          k0' <- k0
          pure $ fold [ k0' ]
        -- 
        C "Group" /\ ps /\ ks -> do
          ks' <- increaseIndentLevel do ks # sequence
          pure $ fold $ fold $ [ [ punctuation "(" ], Array.zipWith (\p k -> do [ p ] <> k) ps ks', [ ps # Array.last # fromMaybe ], [ punctuation ")" ] ]
        -- 
        C "LineBreak" /\ [ _p0 ] /\ [] -> do
          pure $ fold [ linebreak, indentations ctx.indentLevel ]
        C "LineBreak" /\ _ /\ _ -> do
          assembleExpr_default args
        --
        C str /\ [ _p0 ] /\ [] -> do
          pure $ fold [ literal str ]
        -- 
        _ -> assembleExpr_default args
  , printExpr:
      let
        f = case _ of
          Expr { l: Label { con: C "Root" }, kids } -> kids # map f # String.joinWith " "
          Expr { l: Label { con: C "Symbol" }, kids: [ Expr { l: Label { con: C x }, kids: [] } ] } -> x
          Expr { l: Label { con: C "Group" }, kids } -> "(" <> (kids # map f # String.joinWith " ") <> ")"
          Expr { l: Label { con: C "LineBreak" }, kids: [] } -> "\n"
          Expr _ -> "unimplemented"
      in
        f
  }

expr_LineBreak = C "LineBreak" % []

expr_Symbol x = C "Symbol" % [ C x % [] ]

expr_Group = C "Group" % []
zipper_Group = (expr_Group # atPoint (Point { path: Nil, j: Index 0 })).outside # fromSpanContextToZipper

increaseIndentLevel = local \ctx -> ctx { indentLevel = ctx.indentLevel + 1 }

linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation

isValidPoint :: forall r. Expr (Label C r) -> Point -> Boolean
isValidPoint e0 (Point p) = (e.l # getCon) `Set.member` ls
  where
  Expr e = (e0 # atSubExpr p.path).here
  ls = Set.fromFoldable $ fold
    [ [ C "Root" ]
    , [ C "Group" ]
    ]

