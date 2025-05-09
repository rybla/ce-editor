module Editor.Example.Sexp where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Reader (ask, local, runReader)
import Data.Array as Array
import Data.Expr (Expr(..), Fragment(..), Handle(..), Index(..), Point(..), Span(..), atPoint, atSubExpr, fromPointToString, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth, stampTraversable)
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Render (AssembleExpr, RenderArgs)
import Data.Expr.Render as Expr.Render
import Data.Foldable (and, fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe, none)
import Editor.Common (Diagnostic(..), Editor(..), Label(..), StampedLabel, assembleExpr_default, getCon, getId)
import Effect.Class (liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Properties (id) as HP
import Record as Record
import Ui.Editor.Id (freshId)
import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)
import Utility (collapse)

newtype C = C String

derive instance Newtype C _

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
  , getEditMenu: \state -> do
      -- Group
      zipper_Group' <- zipper_Group # stampTraversable
      edit_Group <- Tuple "Group" <$> Expr.Edit.insert (Zipper_Fragment zipper_Group') state
      -- LineBreak
      expr_LineBreak' <- expr_LineBreak # stampTraversable
      edit_LineBreak <- Tuple "LineBreak" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak' ])) state
      -- 
      pure \query -> do
        case query of
          "group" -> do
            pure [ edit_Group ]
          "linebreak" -> do
            pure [ edit_LineBreak ]
          _ -> do
            expr_Symbol' <- expr_Symbol query # stampTraversable
            edit_Symbol <- Tuple "Symbol" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Symbol' ])) state
            pure [ edit_Symbol ]
  , getShortcut: \ki state -> case unit of
      _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] -> do
        expr_LineBreak' <- expr_LineBreak # stampTraversable
        Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak' ])) state
      _ | ki # matchKeyInfoPattern' [ keyEq "(", not_cmd, not_alt ] -> do
        zipper_Group' <- zipper_Group # stampTraversable
        Expr.Edit.insert (Zipper_Fragment zipper_Group') state
      _ ->
        empty
  , isValidHandle: \root handle ->
      case handle of
        Point_Handle p -> and [ isValidPoint root p ]
        SpanH_Handle sh _ -> and [ isValidPoint root p._L, isValidPoint root p._R ]
          where
          p = getEndPoints_SpanH sh
        ZipperH_Handle zh _ -> and [ isValidPoint root p._OL, isValidPoint root p._IL, isValidPoint root p._IR, isValidPoint root p._OR ]
          where
          p = getEndPoints_ZipperH zh
  , assembleExpr: assembleStampedExpr
  , printExpr:
      let
        f = case _ of
          Expr { l: Label { con: C "Root" }, kids } -> kids # map f # String.joinWith "!!"
          Expr { l: Label { con: C "Symbol" }, kids: [ Expr { l: Label { con: C x }, kids: [] } ] } -> x
          Expr { l: Label { con: C "Group" }, kids } -> "(" <> (kids # map f # String.joinWith " ") <> ")"
          Expr { l: Label { con: C "LineBreak" }, kids: [] } -> "\n"
          Expr _ -> "unimplemented"

      in
        f
  , getDiagnostics: \state -> collapse
      [ state.clipboard <#> \frag ->
          Diagnostic
            { title: "Clipboard"
            , content:
                HHK.div [ classes [ "Expr" ] ] $
                  frag
                    # Expr.Render.renderFragment (renderArgs assembleExpr) none
                    # flip runReader
                        { indentLevel: 0
                        }
            }

      ]
  }

assembleStampedExpr :: forall r. AssembleExpr (StampedLabel C r)
assembleStampedExpr args = assembleExpr_helper (args.label # getId) args

assembleExpr :: forall r. AssembleExpr (Label C r)
assembleExpr args = assembleExpr_helper (args.path # show) args

assembleExpr_helper :: forall r. String -> AssembleExpr (Label C r)
assembleExpr_helper id args = do
  ctx <- ask
  case (args.label # getCon) /\ args.points /\ args.kids of
    C "Root" /\ ps /\ ks -> do
      ks' <- ks # sequence
      pure $ fold $ Array.zipWith (\p k -> [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ]
    C "Symbol" /\ [ _p0, _p1 ] /\ [ k0 ] -> do
      k0' <- k0
      pure $ fold [ k0' ]
    C "Group" /\ ps /\ ks -> do
      ks' <- increaseIndentLevel do ks # sequence
      pure $ fold $ fold $
        [ [ tokens_punctuation (id <> "_begin") "(" ]
        , Array.zipWith (\p k -> do [ p ] <> k) ps ks'
        , [ ps # Array.last # fromMaybe ]
        , [ tokens_punctuation (id <> "_end") ")" ]
        ]
    C "LineBreak" /\ [ _p0 ] /\ [] -> do
      pure $ fold
        [ tokens_ghost (id <> "_marker") "⏎"
        , tokens_break (id <> "_break")
        , tokens_indentation ctx.indentLevel (id <> "_indentation")
        ]
    C literal /\ [ _p0 ] /\ [] -> do
      pure $ fold
        [ tokens_literal id literal
        ]
    C _ /\ _ /\ _ -> assembleExpr_default args

renderArgs :: forall r w i. AssembleExpr (Label C r) -> RenderArgs (Label C r) w i
renderArgs assembleExpr' =
  { renderKid
  , renderPoint
  , assembleExpr: assembleExpr'
  }
  where
  renderKid path expr = Expr.Render.renderExpr (renderArgs assembleExpr') path expr

  renderPoint _label p =
    fromPointToString p /\
      HH.div [ classes [ "Point" ], HP.id (fromPointToString p) ]
        [ HH.text " " ]

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

tokens_punctuation key str = [ mk_token key [ "punctuation" ] (pure str) ]

tokens_ghost key str = [ mk_token key [ "ghost" ] (pure str) ]

tokens_break key = [ mk_token key [ "break", "ghost" ] none ]

tokens_indentation n key =
  mk_token key [ "indentation", "ghost" ] (pure "│")
    # Array.replicate n
    # mapWithIndex \i (key' /\ e) -> (key' <> "_" <> show i) /\ e

tokens_literal key str = [ mk_token key [ "literal" ] (pure str) ]

mk_token key cs Nothing = key /\ HH.div [ HP.id key, classes ([ "Token" ] <> cs) ] []
mk_token key cs (Just str) = key /\ HH.div [ HP.id key, classes ([ "Token" ] <> cs) ] [ HH.text str ]

