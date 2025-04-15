module Ui.App where

import Prelude

import Data.Array as Array
import Data.Expr (Expr(..), Handle(..), Path, Point(..), SpanFocus(..), ZipperFocus(..), atSteps, atSubExpr, defaultHandle, getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeIndexes, getIndexesAroundStep, getStep, mkExpr)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Move as Expr.Move
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor.Example.Editor1 (Dat(..), L(..), example_expr, mkL)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Ui.Common (EventListenerInfo, KeyInfo(..), addClass, addEventListenerWithOptions, body, createElement, doc, fromEventToKeyInfo, matchMapKeyInfo, removeClass, shiftKey)
import Utility (fromMaybeM, (:=))
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..))

--------------------------------------------------------------------------------
-- config
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
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  void $ body # renderEditor

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type State =
  { mb_expr :: Ref (Maybe (Expr UiLabel))
  , mb_handle :: Ref (Maybe Handle)
  , mb_dragOrigin :: Ref (Maybe Handle)
  }

type PureLabel = L {}

type UiLabel = L UiExpr

type UiEditor =
  { elem :: Element
  , eventListenerInfos :: Array EventListenerInfo
  }

type UiExpr =
  { elem :: Element
  , eventListenerInfos :: Array EventListenerInfo
  -- indexed by each Point Index
  , uiPoints :: Array UiPoint
  }

type UiPoint =
  { elem :: Element
  , eventListenerInfos :: Array EventListenerInfo
  }

--------------------------------------------------------------------------------
-- renderEditor
--------------------------------------------------------------------------------

renderEditor :: Element -> Effect UiEditor
renderEditor parent = do
  let expr = mkExpr (mkL Root {}) [ config.initialExpr ]
  Console.logShow expr

  state <- newState

  elem <- parent # createElement "div"
  elem # addClass "Editor"

  expr' <- elem # renderExpr state Nil expr
  state.mb_expr := pure expr'

  eventListenerInfos <- sequence
    [ state # eventListenerInfo_stopDrag
    , state # eventListenerInfo_keyboardAction
    ]

  pure { elem, eventListenerInfos }

newState :: Effect State
newState = do
  mb_expr <- Ref.new Nothing
  mb_handle <- Ref.new Nothing
  mb_dragOrigin <- Ref.new Nothing
  pure { mb_expr, mb_handle, mb_dragOrigin }

eventListenerInfo_stopDrag :: State -> Effect EventListenerInfo
eventListenerInfo_stopDrag state = doc # Document.toEventTarget
  # addEventListenerWithOptions (EventType "mouseup") { capture: true, passive: true } \_event -> do
      state.mb_dragOrigin := none

eventListenerInfo_keyboardAction :: State -> Effect EventListenerInfo
eventListenerInfo_keyboardAction state = doc # Document.toEventTarget
  # addEventListenerWithOptions (EventType "keydown") { capture: true } \event -> do
      let KeyInfo ki = event # fromEventToKeyInfo
      case unit of
        -- move
        _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just false, alt: Just false } -> do
          state.mb_handle # Ref.read >>= case _ of
            Nothing -> state # setHandle (Just defaultHandle)
            Just handle -> do
              expr <- state # getExpr
              case Expr.Move.move expr dir handle of
                Nothing -> pure unit
                Just point -> state # setHandle (Just (Point_Handle point))
        -- drag move
        _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just true, alt: Just false } -> do
          state.mb_handle # Ref.read >>= case _ of
            Nothing -> state # setHandle (Just defaultHandle)
            Just handle -> do
              expr <- state # getExpr
              case Expr.Move.moveUntil expr dir handle (\p -> Expr.Drag.drag handle p expr) of
                Nothing -> pure unit
                Just handle' -> state # setHandle (Just handle')
        _ -> pure unit

--------------------------------------------------------------------------------
-- renderExpr
--------------------------------------------------------------------------------

renderExpr :: State -> Path -> Expr PureLabel -> Element -> Effect (Expr UiLabel)
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

  eventListenerInfos <- sequence
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
  kids' /\ uiPoints_init <- map Array.unzip $ Expr expr # atSteps # traverse \{ outside: t, at: kid } -> do
    let i = t # getStep
    let j = (i # getIndexesAroundStep)._L
    uiPoint <- elem_expr # renderPoint state (Point { path, j })
    kid' <- elem_expr # renderExpr state (List.snoc path i) kid
    pure $ kid' /\ uiPoint
  uiPoint_last <- elem_expr # renderPoint state (Point { path, j: (Expr expr # getExtremeIndexes)._R })
  let uiPoints = uiPoints_init `Array.snoc` uiPoint_last

  -- close
  when (config.displayStyle == Inline_DisplayStyle) do
    elem_close <- elem_expr # createElement "div"
    elem_close # addClass "Punctuation"
    elem_close # Element.toNode # Node.setTextContent ")"

  let
    meta_expr :: UiExpr
    meta_expr =
      { elem: elem_expr
      , eventListenerInfos
      , uiPoints
      }

  pure $ Expr
    { l: expr.l # Newtype.over L _ { meta = meta_expr }
    , kids: kids'
    }

--------------------------------------------------------------------------------
-- renderPoint
--------------------------------------------------------------------------------

renderPoint :: State -> Point -> Element -> Effect UiPoint
renderPoint state (Point point) elem_parent = do
  elem <- elem_parent # createElement "div"
  elem # addClass "Point"
  eventListenerInfos <- sequence
    [ elem # Element.toEventTarget # addEventListenerWithOptions (EventType "mousedown") { capture: true, passive: true } \event -> do
        -- Console.log $ "Point: mousedown" <> show (Point point)
        state.mb_handle # Ref.read >>= case _ of
          Just h | event # shiftKey -> do
            expr <- state # getExpr
            state.mb_dragOrigin := pure h
            case Expr.Drag.drag h (Point point) expr of
              Nothing -> pure unit
              Just h' -> state # setHandle (pure h')
          Just h -> do
            expr <- state # getExpr
            let dragOrigin = Expr.Drag.getDragOrigin h (Point point)
            state.mb_dragOrigin := pure dragOrigin
            case Expr.Drag.drag dragOrigin (Point point) expr of
              Nothing -> pure unit
              Just h' -> state # setHandle (pure h')
          _ -> do
            let h' = Point_Handle (Point point)
            state.mb_dragOrigin := pure h'
            state # setHandle (pure h')
    , elem # Element.toEventTarget # addEventListenerWithOptions (EventType "mouseenter") { capture: true, passive: true } \_event -> do
        -- Console.log $ "Point: mouseenter" <> show (Point point)
        state.mb_dragOrigin # Ref.read >>= case _ of
          Nothing -> pure unit
          Just h -> do
            expr <- state # getExpr
            case Expr.Drag.drag h (Point point) expr of
              Nothing -> pure unit
              Just h' -> state # setHandle (pure h')
    ]

  do
    elem_Focus <- elem # createElement "div"
    elem_Focus # addClass "Focus"

    elem_L <- elem # createElement "div"
    elem_L # addClass "Left"

    elem_M <- elem # createElement "div"
    elem_M # addClass "Middle"

    elem_R <- elem # createElement "div"
    elem_R # addClass "Right"

  pure
    { elem
    , eventListenerInfos
    }

--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

modifyHandle :: Boolean -> State -> Maybe Handle -> Effect Unit
modifyHandle b state mb_handle = do
  let modifyClass = if b then addClass else removeClass
  case mb_handle of
    Nothing -> pure unit
    Just (Point_Handle p) -> do
      uiPoint <- getUiPoint state p
      uiPoint.elem # modifyClass "Point_Handle"
      uiPoint.elem # modifyClass "HandleFocus"
    Just (SpanH_Handle h f) -> do
      let p = getEndPoints_SpanH h
      uiPoint_L <- getUiPoint state p._L
      uiPoint_L.elem # modifyClass "SpanH_Handle_Left"
      uiPoint_R <- getUiPoint state p._R
      uiPoint_R.elem # modifyClass "SpanH_Handle_Right"
      case f of
        Left_SpanFocus -> uiPoint_L.elem # modifyClass "HandleFocus"
        Right_SpanFocus -> uiPoint_R.elem # modifyClass "HandleFocus"
    Just (ZipperH_Handle h f) -> do
      let p = getEndPoints_ZipperH h
      uiPoint_OL <- getUiPoint state p._OL
      uiPoint_OL.elem # modifyClass "ZipperH_Handle_OuterLeft"
      uiPoint_IL <- getUiPoint state p._IL
      uiPoint_IL.elem # modifyClass "ZipperH_Handle_InnerLeft"
      uiPoint_IR <- getUiPoint state p._IR
      uiPoint_IR.elem # modifyClass "ZipperH_Handle_InnerRight"
      uiPoint_OR <- getUiPoint state p._OR
      uiPoint_OR.elem # modifyClass "ZipperH_Handle_OuterRight"
      case f of
        OuterLeft_ZipperFocus -> uiPoint_OL.elem # modifyClass "HandleFocus"
        InnerLeft_ZipperFocus -> uiPoint_IL.elem # modifyClass "HandleFocus"
        InnerRight_ZipperFocus -> uiPoint_IR.elem # modifyClass "HandleFocus"
        OuterRight_ZipperFocus -> uiPoint_OR.elem # modifyClass "HandleFocus"

setHandle :: Maybe Handle -> State -> Effect Unit
setHandle mb_handle' state = do
  mb_handle <- state.mb_handle # Ref.read
  modifyHandle false state mb_handle
  modifyHandle true state mb_handle'
  state.mb_handle := mb_handle'

getExpr :: State -> Effect (Expr UiLabel)
getExpr state = state.mb_expr # Ref.read >>= case _ of
  Nothing -> throw $ "getExpr: isNothing state.expr!"
  Just expr -> pure expr

getExprElement :: State -> Path -> Effect Element
getExprElement state path = do
  expr <- getExpr state
  pure (((expr # atSubExpr path).at # unwrap).l # unwrap).meta.elem

getUiPoint :: State -> Point -> Effect UiPoint
getUiPoint state (Point p) = do
  expr <- getExpr state
  let expr' = (expr # atSubExpr p.path).at
  ((expr' # unwrap).l # unwrap).meta.uiPoints Array.!! (unwrap p.j)
    # fromMaybeM (throw "getUiPoint: p.j out-of-bounds")

