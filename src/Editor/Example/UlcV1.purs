module Editor.Example.UlcV1 where

import Data.Expr
import Prelude

import Control.Monad.Reader (ask, local)
import Control.Plus (empty)
import Data.Array as Array
import Data.Expr.Edit as Expr.Edit
import Data.Foldable (and, fold)
import Data.List (List(..), (:))
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe)
import Editor.Common (Editor(..), assembleExpr_default)
import Editor.Notation (keyword, literal, punctuation)
import Halogen.HTML as HH
import Ui.Event (keyEq, matchKeyInfo, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)

{-

t = x
  | (λ x+ . t)
  | (t*)
  | (let x = t in t)

-}

type L = String

editor :: Editor L
editor = Editor
  { name: "UlcV1"
  , initial_expr: "Root" % []
  , initial_handle: Point_Handle $ Point { path: mempty, j: wrap 0 }
  , getEditMenu: \state -> case _ of
      "lam" ->
        [ "LamParams" /\ Expr.Edit.insert (Zipper_Fragment zipper_LamParams) state
        , "LamBody" /\ Expr.Edit.insert (Zipper_Fragment zipper_LamBody) state
        ]
      "app" ->
        [ "App" /\ Expr.Edit.insert (Zipper_Fragment zipper_App) state
        ]
      "let" ->
        [ "LetVar" /\ Expr.Edit.insert (Zipper_Fragment zipper_LetVar) state
        , "LetImpl" /\ Expr.Edit.insert (Zipper_Fragment zipper_LetImpl) state
        , "LetBody" /\ Expr.Edit.insert (Zipper_Fragment zipper_LetBody) state
        , "Var" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_Var "let" ])) state
        ]
      query ->
        [ "Var" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_Var query ])) state
        ]
  , getShortcut: \ki state -> case unit of
      _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] -> do
        Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
      _ | ki # matchKeyInfoPattern' [ keyEq "(", not_cmd, not_alt ] -> do
        Expr.Edit.insert (Zipper_Fragment zipper_App) state
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
        "Root" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ Array.zipWith (\p k -> [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ]

        "LineBreak" /\ [ _p0 ] /\ [] -> do
          pure $ fold [ linebreak, indentations ctx.indentLevel ]
        "LineBreak" /\ _ /\ _ -> do
          assembleExpr_default args

        "Var" /\ [ _p0, _p1 ] /\ [ k0 ] -> do
          k0' <- k0
          pure $ fold [ k0' ]

        "Lam" /\ _ /\ [ k0, k1 ] -> do
          k0' <- increaseIndentLevel do k0
          k1' <- increaseIndentLevel do k1
          pure $ fold [ punctuation "(", keyword "λ", k0', keyword ".", k1', punctuation ")" ]
        "Lam" /\ _ /\ _ -> do
          assembleExpr_default args
        -- 
        "LamParams" /\ [ p ] /\ [] -> do
          pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LamParams" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ]
        -- 
        "LamBody" /\ [ p ] /\ [] -> do
          pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LamBody" /\ [ p0, p1 ] /\ [ k0 ] -> do
          k0' <- k0
          pure $ fold [ [ p0 ], k0', [ p1 ] ]
        "LamBody" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
        -- 
        "App" /\ ps /\ ks -> do
          ks' <- increaseIndentLevel do ks # sequence
          pure $ fold $ fold $ [ [ punctuation "(" ], Array.zipWith (\p k -> do [ p ] <> k) ps ks', [ ps # Array.last # fromMaybe ], [ punctuation ")" ] ]
        -- 
        "Let" /\ _ /\ [ k0, k1, k2 ] -> do
          k0' <- increaseIndentLevel do k0
          k1' <- increaseIndentLevel do k1
          k2' <- increaseIndentLevel do k2
          pure $ fold [ punctuation "(", keyword "let", k0', punctuation "=", k1', keyword "in", k2', punctuation ")" ]
        "Let" /\ _ /\ _ -> do
          assembleExpr_default args
        -- 
        "LetVar" /\ [ p ] /\ [] -> do
          pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LetVar" /\ [ p0, p1 ] /\ [ k0 ] -> do
          k0' <- k0
          pure $ fold [ [ p0 ], k0', [ p1 ] ]
        "LetVar" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
        -- 
        "LetImpl" /\ [ p ] /\ [] -> do
          pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LetImpl" /\ [ p0, p1 ] /\ [ k0 ] -> do
          k0' <- k0
          pure $ fold [ [ p0 ], k0', [ p1 ] ]
        "LetImpl" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
        -- 
        "LetBody" /\ [ p ] /\ [] -> do
          pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LetBody" /\ [ p0, p1 ] /\ [ k0 ] -> do
          k0' <- k0
          pure $ fold [ [ p0 ], k0', [ p1 ] ]
        "LetBody" /\ ps /\ ks -> do
          ks' <- ks # sequence
          pure $ fold $ [ beforeExtraKid ] <> Array.zipWith (\p k -> do [ p ] <> k) ps ks' <> [ ps # Array.last # fromMaybe ] <> [ afterExtraKid ]
        --
        str /\ [ _p0 ] /\ [] -> do
          pure $ fold [ literal str ]
        -- 
        _ -> assembleExpr_default args
  , toString:
      let
        f = case _ of
          Expr { l: "Root", kids } -> kids # map f # String.joinWith "\n"
          Expr { l: "Var", kids: [ Expr { l: x, kids: [] } ] } -> x
          Expr { l: "App", kids } -> "(" <> (kids # map f # String.joinWith " ") <> ")"
          Expr _ -> "unimplemented"
      in
        f
  }

increaseIndentLevel = local \ctx -> ctx { indentLevel = ctx.indentLevel + 1 }

beforeHolePoint = [ HH.div [ classes [ "Token", "beforeHolePoint" ] ] [ HH.text "" ] ]
afterHolePoint = [ HH.div [ classes [ "Token", "afterHolePoint" ] ] [ HH.text "" ] ]
beforeExtraKid = [ HH.div [ classes [ "Token", "beforeExtraKid" ] ] [ HH.text "[" ] ]
afterExtraKid = [ HH.div [ classes [ "Token", "afterExtraKid" ] ] [ HH.text "]" ] ]
linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation

isValidPoint :: Expr L -> Point -> Boolean
isValidPoint expr (Point p) = e'.l `Set.member` ls
  where
  Expr e' = (expr # atSubExpr p.path).here
  ls = Set.fromFoldable $ fold
    [ [ "Root" ]
    , [ "LamParams", "LamBody" ]
    , [ "App" ]
    , [ "LetVar", "LetImpl", "LetBody" ]
    ]

-- LineBreak

expr_LineBreak = "LineBreak" % []

-- Var

expr_Var x = "Var" % [ x % [] ]

-- Lam

expr_Lam = "Lam" % [ "LamParams" % [], "LamBody" % [] ]
zipper_LamParams = (expr_Lam # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LamBody = (expr_Lam # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- App

expr_App = "App" % []
zipper_App = (expr_App # atPoint (Point { path: Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- Let

expr_Let = "Let" % [ "LetVar" % [], "LetImpl" % [], "LetBody" % [] ]
zipper_LetVar = (expr_Let # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetImpl = (expr_Let # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetBody = (expr_Let # atPoint (Point { path: Step 2 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

