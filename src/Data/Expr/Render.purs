module Data.Expr.Render where

import Prelude

import Data.Expr (Expr(..), ExprContext(..), Fragment(..), Path, Point(..), Span(..), SpanContext(..), SpanTooth(..), Tooth(..), Zipper(..), mapIndexes, mapIndexes_SpanTooth, mapIndexes_Tooth, mapStepsAndKids, mapStepsAndKids_SpanTooth, mapStepsAndKids_Tooth)
import Data.Foldable (fold, length)
import Data.List (List(..), (:))
import Data.List as List
import Editor (AssembleExpr, Label)
import Editor.Common (RenderM)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Halogen (classes)

type RenderArgs c w i =
  { renderKid :: Path -> Expr (Label c ()) -> RenderM (Array (HTML w i))
  , renderPoint :: Point -> HTML w i
  , assembleExpr :: AssembleExpr c
  }

renderExpr :: forall w i c. Show c => RenderArgs c w i -> Path -> Expr (Label c ()) -> RenderM (Array (HTML w i))
renderExpr { renderKid, renderPoint, assembleExpr } path (Expr e) =
  assembleExpr
    { label: e.l
    , kids: Expr e # mapStepsAndKids (\i expr' -> renderKid (path `List.snoc` i) expr')
    , points: Expr e # mapIndexes \j -> renderPoint (Point { path, j })
    }

-- -- TODO: nothing other than renderExpr passes the correct Path to recursive
-- -- calls -- but thatr's mostly fine for my purposes since the only place where
-- -- anything other than renderExpr is called is at hte Buffer which doesn't
-- -- render interactive elements into the stuff anyway

renderSpan :: forall w i c. Show c => RenderArgs c w i -> Path -> Span (Label c ()) -> RenderM (Array (HTML w i))
renderSpan args path (Span exprs) = exprs # map (renderExpr args path) # fold

renderZipper :: forall w i c. Show c => RenderArgs c w i -> Path -> Zipper (Label c ()) -> Array (RenderM (Array (HTML w i))) -> RenderM (Array (HTML w i))
renderZipper args path (Zipper z) inside = fold $
  [ fold $ renderExpr args path <$> z.kids_L
  , fold $ [ renderSpanContext args path z.inside inside ]
  , fold $ renderExpr args path <$> z.kids_R
  ]

renderTooth :: forall w i c. Show c => RenderArgs c w i -> Path -> Tooth (Label c ()) -> RenderM (Array (HTML w i)) -> RenderM (Array (HTML w i))
renderTooth args path (Tooth t) inside =
  args.assembleExpr
    { label: t.l
    , kids: Tooth t # mapStepsAndKids_Tooth (\i expr' -> args.renderKid (path `List.snoc` i) expr') inside
    , points: Tooth t # mapIndexes_Tooth (\j -> args.renderPoint (Point { path: path, j }))
    }

renderSpanTooth :: forall w i c. Show c => RenderArgs c w i -> Path -> SpanTooth (Label c ()) -> Array (RenderM (Array (HTML w i))) -> RenderM (Array (HTML w i))
renderSpanTooth args path (SpanTooth st) inside =
  args.assembleExpr
    { label: st.l
    , kids: SpanTooth st # mapStepsAndKids_SpanTooth (\i expr' -> args.renderKid (path `List.snoc` i) expr') inside
    , points: SpanTooth st # mapIndexes_SpanTooth (inside # length) (\j -> args.renderPoint (Point { path: path, j }))
    }

renderExprContext :: forall w i c. Show c => RenderArgs c w i -> Path -> ExprContext (Label c ()) -> RenderM (Array (HTML w i)) -> RenderM (Array (HTML w i))
renderExprContext _args _path (ExprContext Nil) inside = inside
renderExprContext args path (ExprContext (t : ts)) inside = renderTooth args path t $ renderExprContext args path (ExprContext ts) inside

renderSpanContext :: forall w i c. Show c => RenderArgs c w i -> Path -> SpanContext (Label c ()) -> Array (RenderM (Array (HTML w i))) -> RenderM (Array (HTML w i))
renderSpanContext args path (SpanContext sc) inside = renderExprContext args path sc._O $ renderSpanTooth args path sc._I inside

renderFragment :: forall w i c. Show c => RenderArgs c w i -> Path -> Fragment (Label c ()) -> RenderM (Array (HTML w i))
renderFragment args path (Span_Fragment s) = renderSpan args path s
renderFragment args path (Zipper_Fragment z) = renderZipper args path z [ pure [ hole ] ]

hole :: forall w i. HTML w i
hole = HH.div [ classes [ "Hole" ] ] [ HH.text " " ]

