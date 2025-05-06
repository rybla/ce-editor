module Editor.Example.Sexp where

import Prelude

import Control.Monad.Reader (ask, local)
import Control.Plus (empty)
import Data.Array as Array
import Data.Expr (Expr(..), Fragment(..), Handle(..), Index(..), Point(..), Span(..), atPoint, atSubExpr, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, (%))
import Data.Expr.Edit as Expr.Edit
import Data.Foldable (and, fold)
import Data.List (List(..))
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe)
import Editor.Common (Editor(..), assembleExpr_default)
import Editor.Notation (literal, punctuation)
import Halogen.HTML as HH
import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)

newtype L = L String

instance Show L where
  show (L s) = s

derive newtype instance Eq L

derive newtype instance Ord L

editor :: Editor L
editor = Editor
  { name: "Sexp"
  , initialExpr: L "Root" % []
  , initialHandle: Point_Handle $ Point { path: mempty, j: wrap 0 }
  , getEditMenu: \state query -> case query of
      "group" -> [ "Group" /\ Expr.Edit.insert (Zipper_Fragment zipper_Group) state ]
      "linebreak" -> [ "LineBreak" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state ]
      _ -> [ "Symbol" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_Symbol query ])) state ]
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
      case args.label /\ args.points /\ args.kids of
        L "Root" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ Array.zipWith (\p k -> [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ]
        -- 
        L "Symbol" /\ [ _p0, _p1 ] /\ [ k0 ] -> do
          k0' <- k0
          pure $ fold [ k0' ]
        -- 
        L "Group" /\ ps /\ ks -> do
          ks' <- increaseIndentLevel do ks # sequence
          pure $ fold $ fold $ [ [ punctuation "(" ], Array.zipWith (\p k -> do [ p ] <> k) ps ks', [ ps # Array.last # fromMaybe ], [ punctuation ")" ] ]
        -- 
        L "LineBreak" /\ [ _p0 ] /\ [] -> do
          pure $ fold [ linebreak, indentations ctx.indentLevel ]
        L "LineBreak" /\ _ /\ _ -> do
          assembleExpr_default args
        --
        L str /\ [ _p0 ] /\ [] -> do
          pure $ fold [ literal str ]
        -- 
        _ -> assembleExpr_default args
  , printExpr:
      let
        f = case _ of
          Expr { l: L "Root", kids } -> kids # map f # String.joinWith " "
          Expr { l: L "Symbol", kids: [ Expr { l: L x, kids: [] } ] } -> x
          Expr { l: L "Group", kids } -> "(" <> (kids # map f # String.joinWith " ") <> ")"
          Expr { l: L "LineBreak", kids: [] } -> "\n"
          Expr _ -> "unimplemented"
      in
        f
  }

expr_LineBreak = L "LineBreak" % []

expr_Symbol x = L "Symbol" % [ L x % [] ]

expr_Group = L "Group" % []
zipper_Group = (expr_Group # atPoint (Point { path: Nil, j: Index 0 })).outside # fromSpanContextToZipper

increaseIndentLevel = local \ctx -> ctx { indentLevel = ctx.indentLevel + 1 }

linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation

isValidPoint :: Expr L -> Point -> Boolean
isValidPoint e0 (Point p) = e.l `Set.member` ls
  where
  Expr e = (e0 # atSubExpr p.path).here
  ls = Set.fromFoldable $ fold
    [ [ L "Root" ]
    , [ L "Group" ]
    ]

