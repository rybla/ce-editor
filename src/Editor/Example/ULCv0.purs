module Editor.Example.UlcV0 where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Expr (Expr, Fragment(..), Handle(..), Index(..), Point(..), Span(..), Step(..), atPoint, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth)
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Render (AssembleExpr)
import Data.Foldable (and, fold)
import Data.List (List(..), (:))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Editor (Label(..))
import Editor.Common (Editor(..), assembleExpr_default)
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
  { name: "UlcV0"
  , initialExpr: C "Root" % []
  , initialHandle: Point_Handle $ Point { path: mempty, j: wrap 0 }
  -- , getEditMenu: \state query -> collapse case query of
  --     "lam" ->
  --       [ Tuple "LamParams" <$> Expr.Edit.insert (Zipper_Fragment zipper_LamParams) state
  --       , Tuple "LamBody" <$> Expr.Edit.insert (Zipper_Fragment zipper_LamBody) state
  --       , Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var (C query) ])) state
  --       ]
  --     "app" ->
  --       [ Tuple "AppFunc" <$> Expr.Edit.insert (Zipper_Fragment zipper_AppFunc) state
  --       , Tuple "AppArgs" <$> Expr.Edit.insert (Zipper_Fragment zipper_AppArgs) state
  --       , Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var (C query) ])) state
  --       ]
  --     "let" ->
  --       [ Tuple "LetVar" <$> Expr.Edit.insert (Zipper_Fragment zipper_LetVar) state
  --       , Tuple "LetImpl" <$> Expr.Edit.insert (Zipper_Fragment zipper_LetImpl) state
  --       , Tuple "LetBody" <$> Expr.Edit.insert (Zipper_Fragment zipper_LetBody) state
  --       , Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var (C query) ])) state
  --       ]
  --     _ ->
  --       [ Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var (C query) ])) state
  --       , Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var (C query) ])) state
  --       ]
  , getEditMenu: todo ""
  , getShortcut: \ki state -> case unit of
      -- _ | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] ->
      --   Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak ])) state
      _ -> pure none
  , isValidHandle: \root handle -> case handle of
      Point_Handle p -> and [ isValidPoint root p ]
      SpanH_Handle sh _ -> and [ isValidPoint root p._L, isValidPoint root p._R ]
        where
        p = getEndPoints_SpanH sh
      ZipperH_Handle zh _ -> and [ isValidPoint root p._OL, isValidPoint root p._IL, isValidPoint root p._IR, isValidPoint root p._OR ]
        where
        p = getEndPoints_ZipperH zh
  , assembleExpr
  , printExpr: const "unimplemented"
  , stampLabel: \(Label l) -> do
      id <- freshId # liftEffect
      pure $ Label $ l `Record.merge` { id }
  }

assembleExpr :: forall r. AssembleExpr (Label C r)
assembleExpr = assembleExpr_default

beforeHolePoint = [ HH.div [ classes [ "Token", "beforeHolePoint" ] ] [ HH.text "" ] ]
afterHolePoint = [ HH.div [ classes [ "Token", "afterHolePoint" ] ] [ HH.text "" ] ]
beforeExtraKid = [ HH.div [ classes [ "Token", "beforeExtraKid" ] ] [ HH.text "[" ] ]
afterExtraKid = [ HH.div [ classes [ "Token", "afterExtraKid" ] ] [ HH.text "]" ] ]
linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation

isValidPoint :: forall r. Expr (Label C r) -> Point -> Boolean
isValidPoint _ _ = true

-- LineBreak

expr_LineBreak = C "LineBreak" % []

-- Var

expr_Var x = C "Var" % [ x % [] ]

-- Lam

expr_Lam = C "Lam" % [ C "LamParams" % [], C "LamBody" % [] ]
zipper_LamParams = (expr_Lam # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LamBody = (expr_Lam # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- App

expr_App = C "App" % [ C "AppFunc" % [], C "AppArgs" % [] ]
zipper_AppFunc = (expr_App # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_AppArgs = (expr_App # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

-- Let

expr_Let = C "Let" % [ C "LetVar" % [], C "LetImpl" % [], C "LetBody" % [] ]
zipper_LetVar = (expr_Let # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetImpl = (expr_Let # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_LetBody = (expr_Let # atPoint (Point { path: Step 2 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

