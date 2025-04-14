module Ui.App where

import Prelude

import Data.Array as Array
import Data.Expr (Expr(..), Handle(..), Path, Point(..), SpanFocus(..), ZipperFocus(..), atSteps, atSubExpr, getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeIndexes, getIndexesAroundStep, getStep, mkExpr)
import Data.Expr.Drag (drag, getDragOrigin)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Editor.Example.Editor1 (Dat(..), L(..), example_expr, mkL)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Ui.Common (EventListenerInfo, addClass, addEventListenerWithOptions, body, createElement, doc, removeClass)
import Utility (fromMaybeM, (:=))
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..))

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

config =
  { initialExpr: example_expr {} 2 2
  , displayStyle: Inline_DisplayStyle
  }

data DisplayStyle
  = Inline_DisplayStyle
  | Nested_DisplayStyle

derive instance Eq DisplayStyle

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

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

type State =
  { expr :: Ref (Maybe (Expr UiLabel))
  , mb_focus :: Ref (Maybe Handle)
  , mb_dragOrigin :: Ref (Maybe Handle)
  }

newState :: Effect State
newState = do
  expr <- Ref.new Nothing
  mb_focus <- Ref.new Nothing
  mb_dragOrigin <- Ref.new Nothing
  pure { expr, mb_focus, mb_dragOrigin }

main :: Effect Unit
main = do
  let expr = mkExpr (mkL Root {}) [ config.initialExpr ]
  Console.logShow expr

  state <- newState

  elem_editor <- do
    elem_editor <- body # createElement "div"
    elem_editor # addClass "Editor"
    pure elem_editor

  expr' <- elem_editor # renderExpr state Nil expr
  state.expr := pure expr'

  void $ doc # Document.toEventTarget
    # addEventListenerWithOptions (EventType "mouseup") { capture: true, passive: true } \_event -> do
        state.mb_dragOrigin := none

  pure unit

renderExpr :: State -> Path -> Expr PreLabel -> Element -> Effect (Expr UiLabel)
renderExpr state path (Expr expr) elem_parent = do
  Console.log $ "renderExpr: " <> show (Expr expr)
  -- create element
  elem_expr <- elem_parent # createElement "div"

  -- attributes
  elem_expr # addClass "Expr"
  case config.displayStyle of
    Inline_DisplayStyle -> elem_expr # addClass "Inline"
    Nested_DisplayStyle -> elem_expr # addClass "Nested"
  case expr.l of
    L { dat: Root } -> elem_expr # addClass "Root"
    _ -> pure unit

  eventListeners <- sequence
    []

  -- open
  when (config.displayStyle == Inline_DisplayStyle) do
    elem_open <- elem_expr # createElement "div"
    elem_open # addClass "Punctuation"
    elem_open # Element.toNode # Node.setTextContent "("

  -- label
  do
    elem_label <- elem_expr # createElement "div"
    elem_label # addClass "Label"
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

  -- close
  when (config.displayStyle == Inline_DisplayStyle) do
    elem_close <- elem_expr # createElement "div"
    elem_close # addClass "Punctuation"
    elem_close # Element.toNode # Node.setTextContent ")"

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
    [ elem_point # Element.toEventTarget # addEventListenerWithOptions (EventType "mousedown") { capture: true, passive: true } \_event -> do
        -- Console.log $ "Point: mousedown" <> show (Point point)
        -- state.mb_focus # Ref.read >>= case _ of
        --   Nothing -> do
        --     let h' = Point_Handle (Point point)
        --     state.mb_dragOrigin := pure h'
        --     state # setHandle (pure h')
        --   Just h -> do
        --     expr <- state # getExpr
        --     state.mb_dragOrigin := pure (getDragOrigin h (Point point))
        --     case drag h (Point point) expr of
        --       Nothing -> pure unit
        --       Just h' -> state # setHandle (pure h')
        let h' = Point_Handle (Point point)
        state.mb_dragOrigin := pure h'
        state # setHandle (pure h')
    , elem_point # Element.toEventTarget # addEventListenerWithOptions (EventType "mouseenter") { capture: true, passive: true } \_event -> do
        -- Console.log $ "Point: mouseenter" <> show (Point point)
        state.mb_dragOrigin # Ref.read >>= case _ of
          Nothing -> pure unit
          Just h -> do
            expr <- state # getExpr
            case drag h (Point point) expr of
              Nothing -> pure unit
              Just h' -> state # setHandle (pure h')
    ]
  pure $ elem_point /\ eventListeners

--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

modifyHandle :: Boolean -> State -> Maybe Handle -> Effect Unit
modifyHandle b state mb_focus = do
  let modifyClass = if b then addClass else removeClass
  case mb_focus of
    Nothing -> pure unit
    Just (Point_Handle p) -> do
      elem_point <- getPointElement state p
      elem_point # modifyClass "Point_Handle"
      elem_point # modifyClass "HandleFocus"
    Just (SpanH_Handle h f) -> do
      let p = getEndPoints_SpanH h
      elem_point_L <- getPointElement state p._L
      elem_point_L # modifyClass "SpanH_Handle_Left"
      elem_point_R <- getPointElement state p._R
      elem_point_R # modifyClass "SpanH_Handle_Right"
      case f of
        Left_SpanFocus -> elem_point_L # modifyClass "HandleFocus"
        Right_SpanFocus -> elem_point_R # modifyClass "HandleFocus"
    Just (ZipperH_Handle h f) -> do
      let p = getEndPoints_ZipperH h
      elem_point_OL <- getPointElement state p._OL
      elem_point_OL # modifyClass "ZipperH_Handle_OuterLeft"
      elem_point_IL <- getPointElement state p._IL
      elem_point_IL # modifyClass "ZipperH_Handle_InnerLeft"
      elem_point_IR <- getPointElement state p._IR
      elem_point_IR # modifyClass "ZipperH_Handle_InnerRight"
      elem_point_OR <- getPointElement state p._OR
      elem_point_OR # modifyClass "ZipperH_Handle_OuterRight"
      case f of
        OuterLeft_ZipperFocus -> elem_point_OL # modifyClass "HandleFocus"
        InnerLeft_ZipperFocus -> elem_point_IL # modifyClass "HandleFocus"
        InnerRight_ZipperFocus -> elem_point_IR # modifyClass "HandleFocus"
        OuterRight_ZipperFocus -> elem_point_OR # modifyClass "HandleFocus"

setHandle :: Maybe Handle -> State -> Effect Unit
setHandle mb_focus' state = do
  mb_focus <- state.mb_focus # Ref.read
  modifyHandle false state mb_focus
  modifyHandle true state mb_focus'
  state.mb_focus := mb_focus'

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

