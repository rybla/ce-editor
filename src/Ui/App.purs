module Ui.App where

import Prelude

import Data.Array as Array
import Data.Expr (Expr(..), Handle(..), Path, Point(..), atSteps, atSubExpr, getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeIndexes, getIndexesAroundStep, getStep, mkExpr)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Editor.Example.Editor1 (Dat(..), L(..), example_expr, mkL)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Ui.Common (addClass, body, createElement, removeClass)
import Utility (fromMaybeM, (.=))
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, addEventListenerWithOptions, eventListener)

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

config =
  { initialExpr: example_expr {} 2 2
  }

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type PreLabel = L {}

type UiLabel = L UiMeta

type UiMeta =
  { elem :: Element
  , eventListeners :: Array EventListenerInfo
  -- indexed by each Point Index
  , points :: Array (Element /\ Array EventListenerInfo)
  }

type EventListenerInfo =
  { eventType :: EventType
  , eventListener :: EventListener
  , capture :: Boolean
  }

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

type State =
  { expr :: Ref (Maybe (Expr UiLabel))
  , mb_handle :: Ref (Maybe Handle)
  }

newState :: Effect State
newState = do
  expr <- Ref.new Nothing
  mb_handle <- Ref.new Nothing
  pure { expr, mb_handle }

main :: Effect Unit
main = do
  let expr = mkExpr (mkL Root {}) [ example_expr {} 2 2 ]
  Console.logShow expr

  state <- newState

  elem_editor <- do
    elem_editor <- body # createElement "div"
    elem_editor # addClass "Editor"
    pure elem_editor

  expr' <- elem_editor # renderExpr state Nil expr
  state.expr .= pure expr'

  pure unit

renderExpr :: State -> Path -> Expr PreLabel -> Element -> Effect (Expr UiLabel)
renderExpr state path (Expr expr) elem_parent = do
  Console.log $ "renderExpr: " <> show (Expr expr)
  -- create element
  elem_expr <- elem_parent # createElement "div"

  -- attributes
  elem_expr # addClass "Expr"
  case expr.l of
    L { dat: Root } -> elem_expr # addClass "Root"
    _ -> pure unit

  eventListeners <- sequence
    [ do
        let eventType = EventType "click"
        let opts = { capture: true, once: false, passive: true }
        eventListener <- eventListener \_event -> do
          Console.log $ "clicked on Expr: " <> show path
        elem_expr # Element.toEventTarget # addEventListenerWithOptions eventType eventListener opts
        pure { eventType, eventListener, capture: opts.capture }
    ]

  -- label
  do
    elem_label <- elem_expr # createElement "div"
    elem_label # Element.toNode # Node.setTextContent (show expr.l)

  -- kids
  kids' /\ points <- map Array.unzip $ Expr expr # atSteps # traverse \{ outside: t, at: kid } -> do
    let i = t # getStep
    let j = (i # getIndexesAroundStep)._L
    elem_pointoint /\ eventListeners_point <- elem_expr # renderPoint state (Point { path, j })
    kid' <- elem_expr # renderExpr state (List.snoc path i) kid
    pure $ kid' /\ (elem_pointoint /\ eventListeners_point)

  -- last point
  lastPoint <- do
    elem_pointoint /\ eventListeners_points <- elem_expr # renderPoint state (Point { path, j: (Expr expr # getExtremeIndexes)._R })
    pure $ elem_pointoint /\ eventListeners_points

  let
    meta_expr :: UiMeta
    meta_expr =
      { elem: elem_expr
      , eventListeners
      , points: points `Array.snoc` lastPoint
      }

  pure $ Expr
    { l: expr.l # Newtype.over L _ { meta = meta_expr }
    , kids: kids'
    }

--------------------------------------------------------------------------------
-- renderPoint
--------------------------------------------------------------------------------

renderPoint :: State -> Point -> Element -> Effect (Element /\ Array EventListenerInfo)
renderPoint state (Point point) elem_parent = do
  elem_point <- elem_parent # createElement "div"
  elem_point # addClass "Point"
  eventListeners <- sequence
    [ do
        let eventType = EventType "click"
        let opts = { capture: true, once: false, passive: true }
        eventListener <- eventListener \_event -> do
          Console.log $ "clicked on Point: " <> show (Point point)
          setHandle state $ pure $ Point_Handle (Point point)
        elem_point # Element.toEventTarget # addEventListenerWithOptions eventType eventListener opts
        pure { eventType, eventListener, capture: opts.capture }
    ]
  pure $ elem_point /\ eventListeners

--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

modifyHandle :: Boolean -> State -> Maybe Handle -> Effect Unit
modifyHandle b state mb_handle = do
  let modifyClass = if b then addClass else removeClass
  case mb_handle of
    Nothing -> pure unit
    Just (Point_Handle p) -> do
      elem_point <- getPointElement state p
      elem_point # modifyClass "Point_Handle"
    Just (SpanH_Handle h _) -> do
      let p = getEndPoints_SpanH h
      elem_point_L <- getPointElement state p._L
      elem_point_L # modifyClass "SpanH_Handle_L"
      elem_point_R <- getPointElement state p._R
      elem_point_R # modifyClass "SpanH_Handle_R"
    Just (ZipperH_Handle h _) -> do
      let p = getEndPoints_ZipperH h
      elem_point_OL <- getPointElement state p._OL
      elem_point_OL # modifyClass "ZipperH_Handle_OL"
      elem_point_IL <- getPointElement state p._IL
      elem_point_IL # modifyClass "ZipperH_Handle_IL"
      elem_point_IR <- getPointElement state p._IR
      elem_point_IR # modifyClass "ZipperH_Handle_IR"
      elem_point_OR <- getPointElement state p._OR
      elem_point_OR # modifyClass "ZipperH_Handle_OR"

setHandle :: State -> Maybe Handle -> Effect Unit
setHandle state mb_handle' = do
  mb_handle <- state.mb_handle # Ref.read
  modifyHandle false state mb_handle
  modifyHandle true state mb_handle'
  state.mb_handle .= mb_handle'

getExpr :: State -> Effect (Expr UiLabel)
getExpr state = state.expr # Ref.read >>= case _ of
  Nothing -> throw $ "getExpr: isNothing state.expr!"
  Just expr -> pure expr

getExprElement :: State -> Path -> Effect Element
getExprElement state path = do
  expr <- getExpr state
  pure (((expr # atSubExpr path).at # unwrap).l # unwrap).meta.elem

getPointElement :: State -> Point -> Effect Element
getPointElement state (Point p) = do
  expr <- getExpr state
  let expr' = (expr # atSubExpr p.path).at
  ((expr' # unwrap).l # unwrap).meta.points Array.!! (unwrap p.j) # map fst
    # fromMaybeM (throw "getPointElement: p.j out-of-bounds")

