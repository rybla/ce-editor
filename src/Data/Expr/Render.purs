module Data.Expr.Render where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.Either.Nested (type (\/))
import Data.Expr (Expr(..), ExprContext(..), Fragment(..), Path, Point(..), Span(..), SpanContext(..), SpanTooth(..), Step(..), Tooth(..), Zipper(..), mapIndexes, mapIndexes_SpanTooth, mapIndexes_Tooth, mapStepsAndKids, mapStepsAndKids_SpanTooth, mapStepsAndKids_Tooth)
import Data.Foldable (fold, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Halogen.HTML (HTML, PlainHTML)
import Halogen.HTML as HH
import Ui.Halogen (classes)

data Annotation
  = Info_Annotation PlainHTML
  | Error_Annotation PlainHTML

type RenderM = Reader RenderCtx

runRenderM :: forall a. RenderM a -> a
runRenderM = flip runReader
  { indentLevel: 0 -- TODO: change this to 0 when allow feature of specifying which rendering constructs add to indentation level
  }

type RenderCtx =
  { indentLevel :: Int
  }

type KeyHTML w i = String /\ HTML w i

type RenderKid l w i = Maybe l /\ RenderM (Array (KeyHTML w i))

type AssembleExpr l =
  forall w i
   . { path :: Path
     , label :: l
     , kids :: Array (RenderKid l w i)
     , points :: Array (KeyHTML w i)
     }
  -> RenderKid l w i

type RenderArgs l w i =
  { renderKid :: Path -> Expr l -> RenderKid l w i
  , renderPoint :: String \/ l -> Point -> KeyHTML w i
  , assembleExpr :: AssembleExpr l
  }

renderExpr :: forall l w i. Show l => RenderArgs l w i -> Path -> Expr l -> RenderKid l w i
renderExpr { renderKid, renderPoint, assembleExpr } path (Expr e) =
  assembleExpr
    { path
    , label: e.l
    , kids: Expr e # mapStepsAndKids (\i -> renderKid (path `List.snoc` i))
    , points: Expr e # mapIndexes \j -> renderPoint (pure e.l) (Point { path, j })
    }

-- TODO: nothing other than renderExpr passes the correct Path to recursive
-- calls -- but thatr's mostly fine for my purposes since the only place where
-- anything other than renderExpr is called is at hte Buffer which doesn't
-- render interactive elements into the stuff anyway

renderSpan :: forall l w i. Show l => RenderArgs l w i -> Path -> Span l -> Array (RenderKid l w i)
renderSpan args path (Span exprs) =
  exprs
    -- # mapWithIndex
    --     ( \i ->
    --         pure [ args.renderPoint (Left "Span") (Point { path, j: Index i }) ] <>
    --           renderExpr args (path `List.snoc` Step i)
    --     )
    -- # flip append [ pure $ pure $ args.renderPoint (Left "Span") (Point { path, j: Index (exprs # length) }) ]
    # mapWithIndex \i -> renderExpr args (path `List.snoc` Step i)

renderZipper :: forall l w i. Show l => RenderArgs l w i -> Path -> Zipper l -> Array (RenderKid l w i) -> Array (RenderKid l w i)
renderZipper args path (Zipper z) inside = fold $
  [ -- fold $
    --   z.kids_L
    --     # mapWithIndex
    --         ( \i kid ->
    --             pure [ args.renderPoint (Left "Span") (Point { path, j: Index i }) ] <>
    --               renderExpr args (path `List.snoc` Step i) kid
    --         )
    --     # flip append [ pure $ pure $ args.renderPoint (Left "Span") (Point { path, j: Index length_L }) ]
    z.kids_L # mapWithIndex \i -> renderExpr args (path `List.snoc` Step i)
  , [ renderSpanContext args (path `List.snoc` Step length_L) z.inside inside ]
  -- , fold $
  --     z.kids_R
  --       # mapWithIndex
  --           ( \i kid ->
  --               pure [ args.renderPoint (Left "Span") (Point { path, j: Index (length_L + 1 + i) }) ] <>
  --                 renderExpr args (path `List.snoc` Step i) kid
  --           )
  --       # flip append [ pure $ pure $ args.renderPoint (Left "Span") (Point { path, j: Index (length_L + 1 + length_R) }) ]
  , z.kids_R # mapWithIndex \i -> renderExpr args (path `List.snoc` Step (length_L + 1 + i))
  ]
  where
  length_L = z.kids_L # length :: Int
  length_R = z.kids_R # length :: Int

renderTooth :: forall l w i. Show l => RenderArgs l w i -> Path -> Tooth l -> RenderKid l w i -> RenderKid l w i
renderTooth args path (Tooth t) inside =
  args.assembleExpr
    { path
    , label: t.l
    , kids: Tooth t # mapStepsAndKids_Tooth (\i -> args.renderKid (path `List.snoc` i)) inside
    , points: Tooth t # mapIndexes_Tooth (\j -> args.renderPoint (pure t.l) (Point { path: path, j }))
    }

renderSpanTooth :: forall l w i. Show l => RenderArgs l w i -> Path -> SpanTooth l -> Array (RenderKid l w i) -> RenderKid l w i
renderSpanTooth args path (SpanTooth st) inside =
  args.assembleExpr
    { path
    , label: st.l
    , kids: SpanTooth st # mapStepsAndKids_SpanTooth (\i -> args.renderKid (path `List.snoc` i)) inside
    , points: SpanTooth st # mapIndexes_SpanTooth (inside # length) (\j -> args.renderPoint (pure st.l) (Point { path: path, j }))
    }

renderExprContext :: forall l w i. Show l => RenderArgs l w i -> Path -> ExprContext l -> RenderKid l w i -> RenderKid l w i
renderExprContext _args _path (ExprContext Nil) inside = inside
renderExprContext args path (ExprContext (t : ts)) inside = renderTooth args path t $ renderExprContext args path (ExprContext ts) inside

renderSpanContext :: forall l w i. Show l => RenderArgs l w i -> Path -> SpanContext l -> Array (RenderKid l w i) -> RenderKid l w i
renderSpanContext args path (SpanContext sc) inside = renderExprContext args path sc._O $ renderSpanTooth args path sc._I inside

renderFragment :: forall l w i. Show l => RenderArgs l w i -> Path -> Fragment l -> Array (RenderKid l w i)
renderFragment args path (Span_Fragment s) = renderSpan args path s
renderFragment args path (Zipper_Fragment z) = renderZipper args path z [ none /\ pure (hole "Zipper_Fragment") ]

hole :: forall w i. String -> Array (KeyHTML w i)
hole prefix = [ (prefix <> "_Hole") /\ HH.div [ classes [ "Hole" ] ] [ HH.text " " ] ]

