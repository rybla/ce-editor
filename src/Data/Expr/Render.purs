module Data.Expr.Render where

import Data.Expr
import Prelude

import Data.Foldable (fold, length)
import Data.List (List(..), (:))
import Data.List as List
import Editor (AssembleExpr)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Halogen (classes)

type RenderArgs l w i =
  { render_kid :: Path -> Expr l -> HTML w i
  , render_point :: Point -> HTML w i
  , assembleExpr :: AssembleExpr l
  }

renderExpr :: forall w i l. Show l => RenderArgs l w i -> Path -> Expr l -> HTML w i
renderExpr { render_kid, render_point, assembleExpr } path (Expr e) =
  HH.div [ classes [ "Expr" ] ]
    $ assembleExpr
        { label: e.l
        , kids: Expr e # mapStepsAndKids \i expr' -> render_kid (path `List.snoc` i) expr'
        , points: Expr e # mapIndexes \j -> render_point (Point { path, j })
        }

-- TODO: nothing other than renderExpr passes the correct Path to recursive
-- calls -- but thatr's mostly fine for my purposes since the only place where
-- anything other than renderExpr is called is at hte Buffer which doesn't
-- render interactive elements into the stuff anyway

renderSpan :: forall w i l. Show l => RenderArgs l w i -> Path -> Span l -> Array (HTML w i)
renderSpan args path (Span exprs) = exprs # map (renderExpr args path)

renderZipper :: forall w i l. Show l => RenderArgs l w i -> Path -> Zipper l -> Array (HTML w i) -> Array (HTML w i)
renderZipper args path (Zipper z) inside = fold $
  [ renderExpr args path <$> z.kids_L
  , [ renderSpanContext args path z.inside inside ]
  , renderExpr args path <$> z.kids_R
  ]

renderTooth :: forall w i l. Show l => RenderArgs l w i -> Path -> Tooth l -> HTML w i -> HTML w i
renderTooth args path (Tooth t) inside =
  HH.div [ classes [ "Expr" ] ]
    $ args.assembleExpr
        { label: t.l
        , kids: Tooth t # mapStepsAndKids_Tooth (\i expr' -> args.render_kid (path `List.snoc` i) expr') inside
        , points: Tooth t # mapIndexes_Tooth (\j -> args.render_point (Point { path: path, j }))
        }

renderSpanTooth :: forall w i l. Show l => RenderArgs l w i -> Path -> SpanTooth l -> Array (HTML w i) -> HTML w i
renderSpanTooth args path (SpanTooth st) inside =
  HH.div [ classes [ "Expr" ] ]
    $ args.assembleExpr
        { label: st.l
        , kids: SpanTooth st # mapStepsAndKids_SpanTooth (\i expr' -> args.render_kid (path `List.snoc` i) expr') inside
        , points: SpanTooth st # mapIndexes_SpanTooth (inside # length) (\j -> args.render_point (Point { path: path, j }))
        }

renderExprContext :: forall w i l. Show l => RenderArgs l w i -> Path -> ExprContext l -> HTML w i -> HTML w i
renderExprContext _args path (ExprContext Nil) inside = inside
renderExprContext args path (ExprContext (t : ts)) inside = renderTooth args path t $ renderExprContext args path (ExprContext ts) inside

renderSpanContext :: forall w i l. Show l => RenderArgs l w i -> Path -> SpanContext l -> Array (HTML w i) -> HTML w i
renderSpanContext args path (SpanContext sc) inside = renderExprContext args path sc._O $ renderSpanTooth args path sc._I inside

renderFragment :: forall w i l. Show l => RenderArgs l w i -> Path -> Fragment l -> HTML w i
renderFragment args path (Span_Fragment s) = HH.div [ classes [ "Fragment", "SpanFragment" ] ] $ renderSpan args path s
renderFragment args path (Zipper_Fragment z) = HH.div [ classes [ "Fragment", "ZipperFragment" ] ] $ renderZipper args path z [ hole ]

hole :: forall w i. HTML w i
hole = HH.div [ classes [ "Hole" ] ] [ HH.text "?" ]

