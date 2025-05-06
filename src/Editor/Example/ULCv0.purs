module Editor.Example.UlcV0 where

import Data.Expr
import Prelude

import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Array as Array
import Data.Expr.Edit as Expr.Edit
import Data.Foldable (and, fold)
import Data.List (List(..), (:))
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
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
  | (t t+)
  | (let x = t in t)

-}

newtype L = L String

instance Show L where
  show (L s) = s

derive newtype instance Eq L

derive newtype instance Ord L

editor :: Editor L
editor = Editor
  { name: "UlcV0"
  , initialExpr: L "Root" % []
  , initialHandle: Point_Handle $ Point { path: mempty, j: wrap 0 }
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
        [ "Var" /\ Expr.Edit.insert (Span_Fragment (Span [ expr_Var (L query) ])) state
        ]
  , getShortcut: \ki state -> case unit of
      _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] -> do
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
  , assembleExpr: assembleExpr_default
  , printExpr: const "unimplemented"
  }

beforeHolePoint = [ HH.div [ classes [ "Token", "beforeHolePoint" ] ] [ HH.text "" ] ]
afterHolePoint = [ HH.div [ classes [ "Token", "afterHolePoint" ] ] [ HH.text "" ] ]
beforeExtraKid = [ HH.div [ classes [ "Token", "beforeExtraKid" ] ] [ HH.text "[" ] ]
afterExtraKid = [ HH.div [ classes [ "Token", "afterExtraKid" ] ] [ HH.text "]" ] ]
linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation

isValidPoint :: Expr L -> Point -> Boolean
isValidPoint _ _ = true

-- LineBreak

expr_LineBreak = L "LineBreak" % []

-- Var

expr_Var x = L "Var" % [ x % [] ]

-- Lam

expr_Lam = L "Lam" % [ L "LamParams" % [], L "LamBody" % [] ]
zipper_LamParams = (expr_Lam # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LamBody = (expr_Lam # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- App

expr_App = L "App" % [ L "AppFunc" % [], L "AppArgs" % [] ]
zipper_AppFunc = (expr_App # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_AppArgs = (expr_App # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- Let

expr_Let = L "Let" % [ L "LetVar" % [], L "LetImpl" % [], L "LetBody" % [] ]
zipper_LetVar = (expr_Let # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetImpl = (expr_Let # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetBody = (expr_Let # atPoint (Point { path: Step 2 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

