module Editor.Example.Sexp where

import Prelude

import Control.Monad.Reader (ask, local)
import Data.Array as Array
import Data.Expr (Expr(..), Fragment(..), Handle(..), Index(..), Point(..), Span(..), atPoint, atSubExpr, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth)
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Render (AssembleExpr)
import Data.Foldable (and, fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe, none)
import Editor (Label(..), assembleExpr_default, getId)
import Editor.Common (Editor(..), StampedLabel, assembleExpr_default, assembleStampedExpr_default, getCon)
import Editor.Notation (literal, punctuation)
import Effect.Class (liftEffect)
import Halogen.HTML as HH
import Record as Record
import Type.Proxy (Proxy(..))
import Ui.Editor.Id (freshId)
import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)
import Utility (collapse)

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
  , getEditMenu: \state query -> collapse case query of
      "group" ->
        [ Tuple "Symbol" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Symbol query ])) state
        , Tuple "Group" <$> Expr.Edit.insert (Zipper_Fragment zipper_Group) state
        ]
      "linebreak" ->
        [ Tuple "LineBreak" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
        ]
      _ ->
        [ Tuple "Symbol" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Symbol query ])) state
        ]
  , getShortcut: \ki state -> case unit of
      _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] ->
        Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
      _ | ki # matchKeyInfoPattern' [ keyEq "(", not_cmd, not_alt ] ->
        Expr.Edit.insert (Zipper_Fragment zipper_Group) state
      _ ->
        none
  , isValidHandle: \root handle -> case handle of
      Point_Handle p -> and [ isValidPoint root p ]
      SpanH_Handle sh _ -> and [ isValidPoint root p._L, isValidPoint root p._R ]
        where
        p = getEndPoints_SpanH sh
      ZipperH_Handle zh _ -> and [ isValidPoint root p._OL, isValidPoint root p._IL, isValidPoint root p._IR, isValidPoint root p._OR ]
        where
        p = getEndPoints_ZipperH zh
  , assembleExpr: assembleExpr
  , assembleStampedExpr: assembleExpr
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
  , stampLabel: \(Label l) -> do
      id <- freshId # liftEffect
      pure $ Label $ l `Record.merge` { id }
  }

assembleExpr :: forall r. AssembleExpr (Label C r)
assembleExpr = assembleExpr_default

assembleStampedExpr :: forall r. AssembleExpr (StampedLabel C r)
assembleStampedExpr args = do
  ctx <- ask
  let id = args.label # getId
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
      pure $ fold $ fold $
        [ [ [ (id <> "_begin") /\ HH.div [ classes [ "Token punctuation" ] ] [ HH.text "(" ] ] ]
        , Array.zipWith (\p k -> do [ p ] <> k) ps ks'
        , [ ps # Array.last # fromMaybe ]
        , [ [ (id <> "_end") /\ HH.div [ classes [ "Token punctuation" ] ] [ HH.text ")" ] ] ]
        ]
    -- 
    C "LineBreak" /\ [ _p0 ] /\ [] -> do
      pure $ fold
        [ [ (id <> "_marker") /\ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ]
          , (id <> "_break") /\ HH.div [ classes [ "Token break" ] ] []
          ]
        , HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ]
            # Array.replicate ctx.indentLevel
            # mapWithIndex \i e -> (id <> "_indent_" <> show i) /\ e
        ]
    C "LineBreak" /\ _ /\ _ -> do
      assembleExpr_default args
    --
    C str /\ [ _p0 ] /\ [] -> do
      pure
        [ (id <> "_literal") /\ HH.div [ classes [ "Token literal" ] ] [ HH.text str ]
        ]
    --
    _ -> assembleStampedExpr_default args

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

