module Editor.Example.UlcV1 where

import Prelude

import Control.Monad.Reader (ask, local)
import Control.Plus (empty)
import Data.Array as Array
import Data.Expr (Expr(..), Fragment(..), Handle(..), Index(..), Point(..), Span(..), Step(..), atPoint, atSubExpr, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth)
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Render (AssembleExpr)
import Data.Foldable (and, fold)
import Data.List (List(..), (:))
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe, none)
import Editor (Label(..), getCon)
import Editor.Common (Editor(..), assembleExpr_default)
import Editor.Notation (keyword, literal, punctuation)
import Effect.Class (liftEffect)
import Halogen.HTML as HH
import Record as Record
import Ui.Editor.Id (freshId)
import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)
import Utility (collapse, todo)

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
  { name: "UlcV1"
  , initialExpr: C "Root" % []
  , initialHandle: Point_Handle $ Point { path: mempty, j: wrap 0 }
  -- , getEditMenu: \state query -> collapse case query of
  --     "lam" ->
  --       [ Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var query ])) state
  --       , Tuple "LamParams" <$> Expr.Edit.insert (Zipper_Fragment zipper_LamParams) state
  --       , Tuple "LamBody" <$> Expr.Edit.insert (Zipper_Fragment zipper_LamBody) state
  --       ]
  --     "app" ->
  --       [ Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var query ])) state
  --       , Tuple "App" <$> Expr.Edit.insert (Zipper_Fragment zipper_App) state
  --       ]
  --     "let" ->
  --       [ Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var query ])) state
  --       , Tuple "LetVar" <$> Expr.Edit.insert (Zipper_Fragment zipper_LetVar) state
  --       , Tuple "LetImpl" <$> Expr.Edit.insert (Zipper_Fragment zipper_LetImpl) state
  --       , Tuple "LetBody" <$> Expr.Edit.insert (Zipper_Fragment zipper_LetBody) state
  --       ]
  --     _ ->
  --       [ Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var query ])) state
  --       ]
  , getEditMenu: todo ""
  , getShortcut: \ki state -> case unit of
      -- _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] ->
      --   Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
      -- _ | ki # matchKeyInfoPattern' [ keyEq "(", not_cmd, not_alt ] ->
      --   Expr.Edit.insert (Zipper_Fragment zipper_App) state
      _ -> pure none
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
          Expr { l: Label { con: C "LineBreak" }, kids: [] } -> "\n"
          Expr { l: Label { con: C "Var" }, kids: [ Expr { l: Label { con: C x }, kids: [] } ] } -> x
          Expr { l: Label { con: C "App" }, kids } -> "(" <> (kids # map f # String.joinWith " ") <> ")"
          Expr _ -> "unimplemented"
      in
        f
  , stampLabel: \(Label l) -> do
      id <- freshId # liftEffect
      pure $ Label $ l `Record.merge` { id }
  }

assembleExpr :: forall r. AssembleExpr (Label C r)
assembleExpr args = do
  ctx <- ask
  case (args.label # getCon) /\ args.points /\ args.kids of
    -- C "Root" /\ ps /\ ks -> do
    --   ks' <- ks # sequence
    --   pure $ fold $ Array.zipWith (\p k -> [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ]

    -- C "LineBreak" /\ [ _p0 ] /\ [] -> do
    --   pure $ fold [ linebreak, indentations ctx.indentLevel ]
    -- C "LineBreak" /\ _ /\ _ -> do
    --   assembleExpr_default args

    -- C "Var" /\ [ _p0, _p1 ] /\ [ k0 ] -> do
    --   k0' <- k0
    --   pure $ fold [ k0' ]

    -- C "Lam" /\ _ /\ [ k0, k1 ] -> do
    --   k0' <- increaseIndentLevel do k0
    --   k1' <- increaseIndentLevel do k1
    --   pure $ fold [ punctuation "(", keyword "λ", k0', keyword ".", k1', punctuation ")" ]
    -- C "Lam" /\ _ /\ _ -> do
    --   assembleExpr_default args
    -- -- 
    -- C "LamParams" /\ [ p ] /\ [] -> do
    --   pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
    -- C "LamParams" /\ ps /\ ks -> do
    --   ks' <- ks # sequence
    --   pure $ fold $ Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ]
    -- -- 
    -- C "LamBody" /\ [ p ] /\ [] -> do
    --   pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
    -- C "LamBody" /\ [ p0, p1 ] /\ [ k0 ] -> do
    --   k0' <- k0
    --   pure $ fold [ [ p0 ], k0', [ p1 ] ]
    -- C "LamBody" /\ ps /\ ks -> do
    --   ks' <- ks # sequence
    --   pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
    -- -- 
    -- C "App" /\ ps /\ ks -> do
    --   ks' <- increaseIndentLevel do ks # sequence
    --   pure $ fold $ fold $ [ [ punctuation "(" ], Array.zipWith (\p k -> do [ p ] <> k) ps ks', [ ps # Array.last # fromMaybe ], [ punctuation ")" ] ]
    -- -- 
    -- C "Let" /\ _ /\ [ k0, k1, k2 ] -> do
    --   k0' <- increaseIndentLevel do k0
    --   k1' <- increaseIndentLevel do k1
    --   k2' <- increaseIndentLevel do k2
    --   pure $ fold [ punctuation "(", keyword "let", k0', punctuation "=", k1', keyword "in", k2', punctuation ")" ]
    -- C "Let" /\ _ /\ _ -> do
    --   assembleExpr_default args
    -- -- 
    -- C "LetVar" /\ [ p ] /\ [] -> do
    --   pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
    -- C "LetVar" /\ [ p0, p1 ] /\ [ k0 ] -> do
    --   k0' <- k0
    --   pure $ fold [ [ p0 ], k0', [ p1 ] ]
    -- C "LetVar" /\ ps /\ ks -> do
    --   ks' <- ks # sequence
    --   pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
    -- -- 
    -- C "LetImpl" /\ [ p ] /\ [] -> do
    --   pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
    -- C "LetImpl" /\ [ p0, p1 ] /\ [ k0 ] -> do
    --   k0' <- k0
    --   pure $ fold [ [ p0 ], k0', [ p1 ] ]
    -- C "LetImpl" /\ ps /\ ks -> do
    --   ks' <- ks # sequence
    --   pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
    -- -- 
    -- C "LetBody" /\ [ p ] /\ [] -> do
    --   pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
    -- C "LetBody" /\ [ p0, p1 ] /\ [ k0 ] -> do
    --   k0' <- k0
    --   pure $ fold [ [ p0 ], k0', [ p1 ] ]
    -- C "LetBody" /\ ps /\ ks -> do
    --   ks' <- ks # sequence
    --   pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
    -- --
    -- C str /\ [ _p0 ] /\ [] -> do
    --   pure $ fold [ literal str ]
    -- -- 
    _ -> assembleExpr_default args

increaseIndentLevel = local \ctx -> ctx { indentLevel = ctx.indentLevel + 1 }

beforeHolePoint = [ HH.div [ classes [ "Token", "beforeHolePoint" ] ] [ HH.text "" ] ]
afterHolePoint = [ HH.div [ classes [ "Token", "afterHolePoint" ] ] [ HH.text "" ] ]
beforeExtraKid = [ HH.div [ classes [ "Token", "beforeExtraKid" ] ] [ HH.text "[" ] ]
afterExtraKid = [ HH.div [ classes [ "Token", "afterExtraKid" ] ] [ HH.text "]" ] ]
linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation

isValidPoint :: forall r. Expr (Label C r) -> Point -> Boolean
isValidPoint expr (Point p) = (e'.l # getCon) `Set.member` ls
  where
  Expr e' = (expr # atSubExpr p.path).here
  ls = Set.fromFoldable $ fold
    [ [ C "Root" ]
    , [ C "LamParams", C "LamBody" ]
    , [ C "App" ]
    , [ C "LetVar", C "LetImpl", C "LetBody" ]
    ]

-- LineBreak

expr_LineBreak :: Expr (Label C ())
expr_LineBreak = C "LineBreak" % []

-- Var

expr_Var
  :: String
  -> Expr (Label C ())
expr_Var x = C "Var" % [ C x % [] ]

-- Lam

expr_Lam :: Expr (Label C ())
expr_Lam = C "Lam" % [ C "LamParams" % [], C "LamBody" % [] ]

zipper_LamParams = (expr_Lam # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LamBody = (expr_Lam # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- App

expr_App :: Expr (Label C ())
expr_App = C "App" % []

zipper_App = (expr_App # atPoint (Point { path: Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- Let

expr_Let :: Expr (Label C ())
expr_Let = C "Let" % [ C "LetVar" % [], C "LetImpl" % [], C "LetBody" % [] ]

zipper_LetVar = (expr_Let # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetImpl = (expr_Let # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetBody = (expr_Let # atPoint (Point { path: Step 2 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

