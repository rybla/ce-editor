module Editor.Example.ULC where

import Data.Expr
import Prelude

import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Array as Array
import Data.Expr.Edit as Expr.Edit
import Data.Foldable (and, fold)
import Data.List (List(..), (:))
import Data.Newtype (wrap)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe)
import Editor.Common (Editor(..), assembleExpr_default)
import Editor.Notation (keyword, literal, punctuation)
import Halogen.HTML as HH
import Ui.Event (matchKeyInfo)
import Ui.Halogen (classes)

{-

t = x
  | (λ x+ . t)
  | (t t+)
  | (let x = t in t)

-}

type L = String

editor :: Editor L
editor = Editor
  { name: "ULC"
  , initial_expr: "Root" % []
  , initial_handle: Point_Handle $ Point { path: mempty, j: wrap 0 }
  , getEditMenu: \state -> case _ of
      "lam" ->
        [ "LamParams" /\ Expr.Edit.insert (Zipper_Fragment zipper_LamParams) state
        , "LamBody" /\ Expr.Edit.insert (Zipper_Fragment zipper_LamBody) state
        ]
      "app" ->
        [ "AppFunc" /\ Expr.Edit.insert (Zipper_Fragment zipper_AppFunc) state
        , "AppArgs" /\ Expr.Edit.insert (Zipper_Fragment zipper_AppArgs) state
        ]
      "let" ->
        [ "LetVar" /\ Expr.Edit.insert (Zipper_Fragment zipper_LetVar) state
        , "LetImpl" /\ Expr.Edit.insert (Zipper_Fragment zipper_LetImpl) state
        , "LetBody" /\ Expr.Edit.insert (Zipper_Fragment zipper_LetBody) state
        ]
      query ->
        [ "Var" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_Var query ])) state
        ]
  , getShortcut: \ki state -> case unit of
      _ | ki # matchKeyInfo (_ == "Enter") { cmd: pure false, alt: pure false } ->
        Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
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
      kids <- args.kids # sequence
      case args.label /\ args.points /\ kids of
        "Root" /\ ps /\ ks -> pure $ fold $ Array.zipWith (\p k -> [ p ] <> k) ps ks <> [ ps # Array.last # fromMaybe ]

        "LineBreak" /\ [ _p0 ] /\ [] -> pure $ fold [ linebreak, indentations ctx.indentLevel ]
        "LineBreak" /\ _ /\ _ -> assembleExpr_default args

        "Var" /\ [ _p0, _p1 ] /\ [ k0 ] -> pure $ fold [ k0 ]

        "Lam" /\ _ /\ [ k0, k1 ] -> pure $ fold [ punctuation "(", keyword "λ", k0, keyword ".", k1, punctuation ")" ]
        "Lam" /\ _ /\ _ -> assembleExpr_default args
        -- 
        "LamParams" /\ [ p ] /\ [] -> pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LamParams" /\ ps /\ ks -> pure $ fold $ Array.zipWith (\p k -> [ p ] <> k) ps ks <> [ ps # Array.last # fromMaybe ]
        -- 
        "LamBody" /\ [ p ] /\ [] -> pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LamBody" /\ [ p0, p1 ] /\ [ k0 ] -> pure $ fold [ [ p0 ], k0, [ p1 ] ]
        "LamBody" /\ ps /\ ks -> pure $ fold $ Array.zipWith (\p k -> [ p ] <> beforeExtraKid <> k <> afterExtraKid) ps ks <> [ ps # Array.last # fromMaybe ]
        -- 
        "App" /\ _ /\ [ k0, k1 ] -> pure $ fold [ k0, punctuation "(", k1, punctuation ")" ]
        "App" /\ _ /\ _ -> assembleExpr_default args
        -- 
        "AppFunc" /\ [ p ] /\ [] -> pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "AppFunc" /\ [ p0, p1 ] /\ [ k0 ] -> pure $ fold [ [ p0 ], k0, [ p1 ] ]
        "AppFunc" /\ ps /\ ks -> pure $ fold $ fold $ [ Array.zipWith (\p k -> [ p ] <> beforeExtraKid <> k <> afterExtraKid) ps ks, [ ps # Array.last # fromMaybe ] ]
        -- 
        "AppArgs" /\ [ p ] /\ [] -> pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "AppArgs" /\ ps /\ ks -> pure $ fold $ fold $ [ Array.zipWith (\p k -> [ p ] <> k <> punctuation ",") ps (Array.dropEnd 1 ks), [ ps # Array.dropEnd 1 # Array.last # fromMaybe ], [ ks # Array.last # fold ], [ ps # Array.last # fromMaybe ] ]
        -- 
        "Let" /\ _ /\ [ k0, k1, k2 ] -> pure $ fold [ punctuation "(", keyword "let", k0, punctuation "=", k1, keyword "in", k2, punctuation ")" ]
        "Let" /\ _ /\ _ -> assembleExpr_default args
        -- 
        "LetVar" /\ [ p ] /\ [] -> pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LetVar" /\ [ p0, p1 ] /\ [ k0 ] -> pure $ fold [ [ p0 ], k0, [ p1 ] ]
        "LetVar" /\ ps /\ ks -> pure $ fold $ Array.zipWith (\p k -> [ p ] <> beforeExtraKid <> k <> afterExtraKid) ps ks <> [ ps # Array.last # fromMaybe ]
        -- 
        "LetImpl" /\ [ p ] /\ [] -> pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LetImpl" /\ [ p0, p1 ] /\ [ k0 ] -> pure $ fold [ [ p0 ], k0, [ p1 ] ]
        "LetImpl" /\ ps /\ ks -> pure $ fold $ Array.zipWith (\p k -> [ p ] <> beforeExtraKid <> k <> afterExtraKid) ps ks <> [ ps # Array.last # fromMaybe ]
        -- 
        "LetBody" /\ [ p ] /\ [] -> pure $ fold [ beforeHolePoint, [ p ], afterHolePoint ]
        "LetBody" /\ [ p0, p1 ] /\ [ k0 ] -> pure $ fold [ [ p0 ], k0, [ p1 ] ]
        "LetBody" /\ ps /\ ks -> pure $ fold $ Array.zipWith (\p k -> [ p ] <> beforeExtraKid <> k <> afterExtraKid) ps ks <> [ ps # Array.last # fromMaybe ]
        --
        str /\ [ _p0 ] /\ [] -> pure $ fold [ literal str ]
        -- 
        _ -> assembleExpr_default args
  }

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
    , [ "AppFunc", "AppArgs" ]
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

expr_App = "App" % [ "AppFunc" % [], "AppArgs" % [] ]
zipper_AppFunc = (expr_App # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_AppArgs = (expr_App # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- Let

expr_Let = "Let" % [ "LetVar" % [], "LetImpl" % [], "LetBody" % [] ]
zipper_LetVar = (expr_Let # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetImpl = (expr_Let # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetBody = (expr_Let # atPoint (Point { path: Step 2 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

