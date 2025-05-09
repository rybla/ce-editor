module Data.Expr.Render where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.Expr (Expr(..), ExprContext(..), Fragment(..), Path, Point(..), Span(..), SpanContext(..), SpanTooth(..), Step(..), Tooth(..), Zipper(..), mapIndexes, mapIndexes_SpanTooth, mapIndexes_Tooth, mapStepsAndKids, mapStepsAndKids_SpanTooth, mapStepsAndKids_Tooth)
import Data.Foldable (fold, intercalate, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Halogen (classes)

type RenderM = Reader RenderCtx

runRenderM :: forall a. RenderM a -> a
runRenderM = flip runReader
  { indentLevel: 0 -- TODO: change this to 0 when allow feature of specifying which rendering constructs add to indentation level
  }

type RenderCtx =
  { indentLevel :: Int
  }

type KeyHTML w i = String /\ HTML w i

type AssembleExpr l =
  forall w i
   . { path :: Path
     , label :: l
     , kids :: Array (RenderM (Array (KeyHTML w i)))
     , points :: Array (KeyHTML w i)
     }
  -> RenderM (Array (KeyHTML w i))

type RenderArgs l w i =
  { renderKid :: Path -> Expr l -> RenderM (Array (KeyHTML w i))
  , renderPoint :: l -> Point -> KeyHTML w i
  , assembleExpr :: AssembleExpr l
  }

renderExpr :: forall l w i. Show l => RenderArgs l w i -> Path -> Expr l -> RenderM (Array (KeyHTML w i))
renderExpr { renderKid, renderPoint, assembleExpr } path (Expr e) =
  assembleExpr
    { path
    , label: e.l
    , kids: Expr e # mapStepsAndKids (\i expr' -> renderKid (path `List.snoc` i) expr')
    , points: Expr e # mapIndexes \j -> renderPoint e.l (Point { path, j })
    }

-- -- TODO: nothing other than renderExpr passes the correct Path to recursive
-- -- calls -- but thatr's mostly fine for my purposes since the only place where
-- -- anything other than renderExpr is called is at hte Buffer which doesn't
-- -- render interactive elements into the stuff anyway

renderSpan :: forall l w i. Show l => RenderArgs l w i -> Path -> Span l -> RenderM (Array (KeyHTML w i))
renderSpan args path (Span exprs) =
  exprs
    # mapWithIndex (\i kid -> renderExpr args (path `List.snoc` Step i) kid)
    # fold -- TODO intercalate points between items 

renderZipper :: forall l w i. Show l => RenderArgs l w i -> Path -> Zipper l -> Array (RenderM (Array (KeyHTML w i))) -> RenderM (Array (KeyHTML w i))
renderZipper args path (Zipper z) inside = fold $
  [ fold $ renderExpr args path <$> z.kids_L
  , fold $ [ renderSpanContext args path z.inside inside ]
  , fold $ renderExpr args path <$> z.kids_R
  ]

renderTooth :: forall l w i. Show l => RenderArgs l w i -> Path -> Tooth l -> RenderM (Array (KeyHTML w i)) -> RenderM (Array (KeyHTML w i))
renderTooth args path (Tooth t) inside =
  args.assembleExpr
    { path
    , label: t.l
    , kids: Tooth t # mapStepsAndKids_Tooth (\i expr' -> args.renderKid (path `List.snoc` i) expr') inside
    , points: Tooth t # mapIndexes_Tooth (\j -> args.renderPoint t.l (Point { path: path, j }))
    }

renderSpanTooth :: forall l w i. Show l => RenderArgs l w i -> Path -> SpanTooth l -> Array (RenderM (Array (KeyHTML w i))) -> RenderM (Array (KeyHTML w i))
renderSpanTooth args path (SpanTooth st) inside =
  args.assembleExpr
    { path
    , label: st.l
    , kids: SpanTooth st # mapStepsAndKids_SpanTooth (\i expr' -> args.renderKid (path `List.snoc` i) expr') inside
    , points: SpanTooth st # mapIndexes_SpanTooth (inside # length) (\j -> args.renderPoint st.l (Point { path: path, j }))
    }

renderExprContext :: forall l w i. Show l => RenderArgs l w i -> Path -> ExprContext l -> RenderM (Array (KeyHTML w i)) -> RenderM (Array (KeyHTML w i))
renderExprContext _args _path (ExprContext Nil) inside = inside
renderExprContext args path (ExprContext (t : ts)) inside = renderTooth args path t $ renderExprContext args path (ExprContext ts) inside

renderSpanContext :: forall l w i. Show l => RenderArgs l w i -> Path -> SpanContext l -> Array (RenderM (Array (KeyHTML w i))) -> RenderM (Array (KeyHTML w i))
renderSpanContext args path (SpanContext sc) inside = renderExprContext args path sc._O $ renderSpanTooth args path sc._I inside

renderFragment :: forall l w i. Show l => RenderArgs l w i -> Path -> Fragment l -> RenderM (Array (KeyHTML w i))
renderFragment args path (Span_Fragment s) = renderSpan args path s
renderFragment args path (Zipper_Fragment z) = renderZipper args path z [ pure (hole "Zipper_Fragment") ]

hole :: forall w i. String -> Array (KeyHTML w i)
hole prefix = [ (prefix <> "_Hole") /\ HH.div [ classes [ "Hole" ] ] [ HH.text " " ] ]

