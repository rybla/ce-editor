module Ui.Editor where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (get, gets, modify_, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (fold)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Expr (Expr(..), (%), (|:))
import Data.Expr as Expr
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Editor (Editor)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Type.Prelude (Proxy(..))
import Ui.Common (column, list, style, text)
import Ui.Console as Console
import Utility (allEqual, todo)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEventType

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

data Query a = OtherQuery a

type Input = Editor

type State =
  { editor :: Editor
  }

data Action
  = Initialize
  | ExprOutput_Action ExprOutput
  | EngineOutput_Action EngineOutput
  | EngineQuery_Action (forall a. a -> EngineQuery a)

type Slots =
  ( "Engine" :: H.Slot EngineQuery EngineOutput Unit
  , "Expr" :: H.Slot ExprQuery ExprOutput Unit
  )

data Output = TellConsole (forall a. a -> Console.Query a)

type M' = ExceptT PlainHTML M
type M = H.HalogenM State Action Slots Output Aff
type Html = H.ComponentHTML Action Slots M

component :: H.Component Query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState editor =
    { editor
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = \action -> do
        state <- get
        handleAction action # runExceptT >>= case _ of
          Left err -> do
            put state
            trace "Editor . Error" err
          Right it -> pure it
    }

  render state =
    let
      initial_expr = Expr.Root % state.editor.initial_exprs
    in
      HH.div
        [ style do
            tell [ "flex-grow: 1", "flex-shrink: 1" ]
            tell [ "overflow: scroll" ]
            tell [ "padding: 0.5em" ]
        ]
        [ HH.slot (Proxy @"Engine") unit engine_component
            { editor: state.editor
            , expr: initial_expr
            , handle: state.editor.initial_handle
            }
            EngineOutput_Action
        , HH.slot (Proxy @"Expr") unit expr_component
            { expr: initial_expr
            }
            ExprOutput_Action
        ]

handleAction :: Action -> M' Unit
handleAction Initialize = do
  lift $ trace "Editor" $ text "initialized"
  doc <- liftEffect $ HTML.Window.document =<< HTML.window
  lift $ H.subscribe' \_sub_id -> HQE.eventListener MouseEventType.mouseup (HTMLDocument.toEventTarget doc) $ MouseEvent.fromEvent >=> \_e -> do
    pure $ EngineQuery_Action EndDrag_EngineQuery
handleAction (EngineQuery_Action q) = do
  lift $ H.tell (Proxy @"Engine") unit q
handleAction (EngineOutput_Action eo) = case eo of
  Output_EngineOutput o -> H.raise o # lift
  ExprQuery_EngineOutput q -> H.tell (Proxy @"Expr") unit q # lift
handleAction (ExprOutput_Action (is /\ eo)) = case eo of
  Output_ExprOutput o ->
    H.raise o # lift
  ExprInteraction pi -> do
    lift $ H.tell (Proxy @"Engine") unit (ExprInteraction_EngineQuery is pi)
  PointInteraction_ExprOutput i pi ->
    lift $ H.tell (Proxy @"Engine") unit (PointInteraction_EngineQuery (Expr.Point is i) pi)

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

data EngineQuery a
  = ExprInteraction_EngineQuery Expr.Path ExprInteraction a
  | PointInteraction_EngineQuery Expr.Point PointInteraction a
  | EndDrag_EngineQuery a

type EngineInput =
  { editor :: Editor
  , expr :: Expr
  , handle :: Expr.Handle
  }

type EngineState =
  { editor :: Editor
  , expr :: Expr
  , handle :: Expr.Handle
  , -- when dragging, this is the handle where the drag originated from
    drag_origin_handle :: Maybe Expr.Handle
  }

data EngineAction
  = InitializeEngine
  | ReceiveEngine EngineInput

type EngineSlots = () :: Row Type

data EngineOutput
  = Output_EngineOutput Output
  | ExprQuery_EngineOutput (forall a. a -> ExprQuery a)

type EngineHTML = H.ComponentHTML EngineAction EngineSlots EngineM
type EngineM' = ExceptT PlainHTML EngineM
type EngineM = H.HalogenM EngineState EngineAction EngineSlots EngineOutput Aff

engine_component :: H.Component EngineQuery EngineInput EngineOutput Aff
engine_component = H.mkComponent { initialState: initialEngineState, eval, render }
  where
  eval = H.mkEval H.defaultEval
    { initialize = pure InitializeEngine
    , receive = pure <<< ReceiveEngine
    , handleQuery = \query -> do
        state <- get
        handleEngineQuery query # runExceptT >>= case _ of
          Left err -> do
            put state
            traceEngineM "Editor . Engine . Error" err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleEngineAction action # runExceptT >>= case _ of
          Left err -> do
            put state
            traceEngineM "Editor . Engine . Error" err
          Right it -> pure it
    }

  render = const $ HH.div [ style do tell [ "display: hidden" ] ] []

initialEngineState :: EngineInput -> EngineState
initialEngineState input =
  { editor: input.editor
  , expr: input.expr
  , handle: input.handle
  , drag_origin_handle: Nothing
  }

handleEngineQuery :: forall a. EngineQuery a -> EngineM' a
handleEngineQuery (ExprInteraction_EngineQuery is ei a) = case ei of
  ClickExpr _event -> do
    lift $ traceEngineM "Engine" $ text $ "got Click from Expr at " <> show "TODO: (Array.fromFoldable is)"
    case List.unsnoc (unwrap is) of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        let l /\ r = Expr.getIndicesAroundStep last
        let h = Expr.mkCursorHandle (Expr.Cursor (Expr.Path init) l r Expr.Left_CursorFocus)
        setHandle h
        pure unit
    pure a
handleEngineQuery (PointInteraction_EngineQuery p pi a) = case pi of
  StartDrag_PointInteraction _event -> do
    lift $ traceEngineM "Drag" $ text $ "got StartDrag from Point at " <> show p
    h <- gets _.handle
    let h' = Expr.getDragOrigin h p
    modify_ _ { drag_origin_handle = Just h' }
    setHandle h'
    pure a
  MidDrag_PointInteraction _event -> do
    lift $ traceEngineM "Drag" $ text $ "got MidDrag from Point at " <> show p
    updateDragToPoint p
    pure a
handleEngineQuery (EndDrag_EngineQuery a) = do
  modify_ _ { drag_origin_handle = Nothing }
  pure a

handleEngineAction :: EngineAction -> EngineM' Unit
handleEngineAction InitializeEngine = do
  lift $ traceEngineM "Editor . Engine" $ text "initialized"
handleEngineAction (ReceiveEngine input) = do
  lift $ traceEngineM "Editor . Engine" $ text "received"
  put $ initialEngineState input

--------------------------------------------------------------------------------

updateDragToPoint :: Expr.Point -> EngineM' Unit
updateDragToPoint p = do
  gets _.drag_origin_handle >>= case _ of
    Nothing -> pure unit
    Just drag_origin_handle -> case Expr.getHandleFromTo drag_origin_handle p of
      Nothing -> do
        lift $ traceEngineM "Drag" $
          column
            [ text $ "updateDragToPoint (failure)"
            , list
                [ text $ "drag_origin_handle = " <> show drag_origin_handle
                , text $ "                 p = " <> show p
                ]
            ]
        pure unit
      Just h' -> do
        lift $ traceEngineM "Drag" $
          column
            [ text $ "updateDragToPoint (success)"
            , list
                [ text $ "drag_origin_handle = " <> show drag_origin_handle
                , text $ "                 p = " <> show p
                , text $ "getHandleFromTo ==> " <> show h'
                ]
            ]
        setHandle h'

setHandle :: Expr.Handle -> EngineM' Unit
setHandle h = do
  deactivateHandle =<< gets _.handle
  modify_ _ { handle = h }
  activateHandle h

setPointStyle :: Expr.Point -> PointStyle -> EngineM' Unit
setPointStyle (Expr.Point is i) style = H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is $ PointQuery_ExprQuery i $ ModifyPointState (_ { style = style }) a') # lift

activateHandle :: Expr.Handle -> EngineM' Unit
activateHandle h | Just p <- Expr.toPointHandle h = do
  setPointStyle p PointCursor_PointStyle
activateHandle h | Just c <- Expr.toCursorHandle h = do
  setPointStyle (Expr.getLeftPoint c) CursorLeft_PointStyle
  setPointStyle (Expr.getRightPoint c) CursorRight_PointStyle
-- TODO: I probably need some more Select*_PointStyles to cover some of these cases
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | is_I == Expr.Path Nil, allEqual [ j_OL, j_IL, j_IR ] = todo "activateHandle"
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | is_I == Expr.Path Nil, allEqual [ j_IL, j_IR, j_OR ] = todo "activateHandle"
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | is_I == Expr.Path Nil, allEqual [ j_IL, j_IR ] = todo "activateHandle"
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | is_I == Expr.Path Nil, allEqual [ j_OL, j_IL ] = todo "activateHandle"
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | is_I == Expr.Path Nil, allEqual [ j_IR, j_OR ] = todo "activateHandle"
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | is_I == Expr.Path Nil = todo "activateHandle"
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | allEqual [ j_OL, j_IL ] = todo "activateHandle"
activateHandle (Expr.Handle is_O j_OL j_OR is_I j_IL j_IR f) | allEqual [ j_IR, j_OR ] = todo "activateHandle"
activateHandle h@(Expr.Handle _is_O _j_OL _j_OR _is_I _j_IL _j_IR _f) = do
  let p_OL /\ p_IL /\ p_IR /\ p_OR = Expr.getPointsOfHandle h
  setPointStyle p_OL SelectOuterLeft_PointStyle
  setPointStyle p_IL SelectInnerLeft_PointStyle
  setPointStyle p_IR SelectInnerRight_PointStyle
  setPointStyle p_OR SelectOuterRight_PointStyle

deactivateHandle :: Expr.Handle -> EngineM' Unit
deactivateHandle h | Just p <- Expr.toPointHandle h = do
  setPointStyle p Normal_PointStyle
deactivateHandle h | Just c <- Expr.toCursorHandle h = do
  setPointStyle (Expr.getLeftPoint c) Normal_PointStyle
  setPointStyle (Expr.getRightPoint c) Normal_PointStyle
deactivateHandle _h = todo "deactivateHandle"

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

data ExprQuery a = ExprQuery Expr.Path (ExprQuery' a)
data ExprQuery' a
  = ModifyExprState (ExprState -> ExprState) a
  | PointQuery_ExprQuery Expr.Index (PointQuery a)

type ExprInput =
  { expr :: Expr
  }

type ExprState =
  { expr :: Expr
  , ping :: Boolean
  }

data ExprAction
  = InitializeExpr
  | ReceiveExpr ExprInput
  | ExprOutput_ExprAction Expr.Step ExprOutput
  | ExprInteraction_ExprAction ExprInteraction
  | PointOutput_ExprAction Expr.Index PointOutput

-- | ClickExpr MouseEvent

type ExprSlots =
  ( "Expr" :: H.Slot ExprQuery ExprOutput Expr.Step
  , "Point" :: H.Slot PointQuery PointOutput Expr.Index
  )

type ExprOutput = Expr.Path /\ ExprOutput'

data ExprOutput'
  = Output_ExprOutput Output
  | ExprInteraction ExprInteraction
  | PointInteraction_ExprOutput Expr.Index PointInteraction

data ExprInteraction = ClickExpr MouseEvent

type ExprHTML = H.ComponentHTML ExprAction ExprSlots ExprM
type ExprM' = ExceptT (Maybe PlainHTML) ExprM
type ExprM = H.HalogenM ExprState ExprAction ExprSlots ExprOutput Aff

expr_component :: H.Component ExprQuery ExprInput ExprOutput Aff
expr_component = H.mkComponent { initialState: initialExprState, eval, render }
  where
  eval = H.mkEval H.defaultEval
    { initialize = pure InitializeExpr
    , receive = pure <<< ReceiveExpr
    , handleQuery = \query -> do
        state <- get
        handleExprQuery query # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> traceExprM "Editor . Expr . Error" err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleExprAction action # runExceptT >>= case _ of
          Left mb_err -> do
            case mb_err of
              Nothing -> pure unit
              Just err -> traceExprM "Editor . Expr . Error" err
            put state
          Right it -> pure it
    }

  render
    { expr: Expr l es
    , ping
    } =
    HH.div
      [ HP.classes $ [ [ HH.ClassName "Expr" ], if ping then [ H.ClassName "ping" ] else [] ] # fold
      -- , HE.onClick (ClickExpr >>> ExprInteraction_ExprAction)
      ]
      ( fold
          [ [ HH.div [ HP.classes [ HH.ClassName "ExprLabel" ] ]
                [ case l of
                    Expr.String s -> text s
                    Expr.Root -> text "root"
                ]
            ]
          , let
              renderPoint i =
                HH.slot (Proxy @"Point") i point_component
                  {}
                  (PointOutput_ExprAction i)
              renderKid i e =
                HH.slot (Proxy @"Expr") i expr_component
                  { expr: e }
                  (ExprOutput_ExprAction i)
            in
              fold
                [ es # foldMapWithIndex \i e ->
                    [ renderPoint (Expr.Index i)
                    , renderKid (Expr.Step i) e
                    ]
                , [ renderPoint (Expr.Index (Array.length es)) ]
                ]
          ]
      )

initialExprState :: ExprInput -> ExprState
initialExprState { expr } =
  { expr
  , ping: false
  }

handleExprQuery :: forall a. ExprQuery a -> ExprM' a
handleExprQuery (ExprQuery (Expr.Path (i : is)) q) = H.query (Proxy @"Expr") i (ExprQuery (Expr.Path is) q) # lift >>= case _ of
  Nothing -> throwError none
  Just a -> pure a
handleExprQuery (ExprQuery (Expr.Path Nil) q) = case q of
  ModifyExprState f a -> do
    modify_ f
    pure a
  PointQuery_ExprQuery i pq -> do
    H.query (Proxy @"Point") i pq # lift >>= case _ of
      Nothing -> throwError none
      Just a -> pure a

handleExprAction :: ExprAction -> ExprM' Unit
handleExprAction InitializeExpr = pure unit
handleExprAction (ReceiveExpr input) = do
  state <- get
  let state' = initialExprState input
  when (state /= state') do
    put state
    pingExpr
-- kid Expr stuff
handleExprAction (ExprOutput_ExprAction i (is /\ o)) = do
  H.raise ((i |: is) /\ o) # lift
handleExprAction (ExprInteraction_ExprAction ei) = do
  case ei of
    ClickExpr event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (Expr.Path Nil /\ ExprInteraction ei) # lift
  pingExpr
-- Point stuff
handleExprAction (PointOutput_ExprAction _i (Output_PointOutput o)) =
  H.raise (Expr.Path Nil /\ Output_ExprOutput o) # lift
handleExprAction (PointOutput_ExprAction i (PointInteraction pi)) = do
  case pi of
    StartDrag_PointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    MidDrag_PointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (Expr.Path Nil /\ PointInteraction_ExprOutput i pi) # lift

pingExpr :: ExprM' Unit
pingExpr = do
  modify_ _ { ping = true }
  Aff.delay (Milliseconds 500.0) # liftAff
  modify_ _ { ping = false }

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

data PointQuery a = ModifyPointState (PointState -> PointState) a

type PointInput = {}

type PointState =
  { style :: PointStyle
  }

data PointStyle
  = Normal_PointStyle
  | PointCursor_PointStyle
  | CursorLeft_PointStyle
  | CursorRight_PointStyle
  | SelectOuterLeft_PointStyle
  | SelectInnerLeft_PointStyle
  | SelectInnerRight_PointStyle
  | SelectOuterRight_PointStyle

derive instance Generic PointStyle _

instance Eq PointStyle where
  eq x = genericEq x

data PointAction
  = InitializePoint
  | ReceivePoint PointInput
  | PointInteraction_PointAction PointInteraction

type PointSlots = () :: Row Type

data PointOutput
  = Output_PointOutput Output
  | PointInteraction PointInteraction

data PointInteraction
  = StartDrag_PointInteraction MouseEvent
  | MidDrag_PointInteraction MouseEvent

type PointHTML = H.ComponentHTML PointAction PointSlots PointM
type PointM' = ExceptT (Maybe PlainHTML) PointM
type PointM = H.HalogenM PointState PointAction PointSlots PointOutput Aff

point_component :: H.Component PointQuery PointInput PointOutput Aff
point_component = H.mkComponent { initialState, eval, render }
  where
  initialState = initialPointStyle

  eval = H.mkEval H.defaultEval
    { initialize = pure InitializePoint
    , receive = pure <<< ReceivePoint
    , handleQuery = \query -> do
        state <- get
        handlePointQuery query # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> tracePointM "Editor . Point . Error" err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handlePointAction action # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> tracePointM "Editor . Point . Error" err
          Right it -> pure it
    }

  render state =
    HH.div
      [ HP.classes $ fold
          [ [ HH.ClassName "Point" ]
          , case state.style of
              Normal_PointStyle -> []
              PointCursor_PointStyle -> [ HH.ClassName "PointCursor" ]
              CursorLeft_PointStyle -> [ HH.ClassName "CursorLeft" ]
              CursorRight_PointStyle -> [ HH.ClassName "CursorRight" ]
              SelectOuterRight_PointStyle -> [ HH.ClassName "SelectOuterRight" ]
              SelectInnerLeft_PointStyle -> [ HH.ClassName "SelectInnerLeft" ]
              SelectInnerRight_PointStyle -> [ HH.ClassName "SelectInnerRight" ]
              SelectOuterLeft_PointStyle -> [ HH.ClassName "SelectOuterLeft" ]
          ]
      , HE.onMouseDown (StartDrag_PointInteraction >>> PointInteraction_PointAction)
      , HE.onMouseEnter (MidDrag_PointInteraction >>> PointInteraction_PointAction)
      ]
      [ text " " ]

initialPointStyle :: PointInput -> PointState
initialPointStyle _input =
  { style: Normal_PointStyle
  }

handlePointQuery :: forall a. PointQuery a -> PointM' a
handlePointQuery (ModifyPointState f a) = do
  modify_ f
  pure a

handlePointAction :: PointAction -> PointM' Unit
handlePointAction InitializePoint = do
  pure unit
handlePointAction (ReceivePoint input) = do
  state <- get
  let state' = initialPointStyle input
  when (state' /= state) do
    put state'
handlePointAction (PointInteraction_PointAction pi) = do
  H.raise (PointInteraction pi) # lift

--------------------------------------------------------------------------------

trace :: String -> PlainHTML -> M Unit
trace label content = H.raise $ TellConsole \a -> Console.AddMessage { label, content } a

traceEngineM :: String -> PlainHTML -> EngineM Unit
traceEngineM label content = H.raise $ Output_EngineOutput $ TellConsole \a -> Console.AddMessage { label, content } a

traceExprM :: String -> PlainHTML -> ExprM Unit
traceExprM label content = H.raise $ Tuple (Expr.Path Nil) $ Output_ExprOutput $ TellConsole \a -> Console.AddMessage { label, content } a

tracePointM :: String -> PlainHTML -> PointM Unit
tracePointM label content = H.raise $ Output_PointOutput $ TellConsole \a -> Console.AddMessage { label, content } a

