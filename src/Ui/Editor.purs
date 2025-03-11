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
import Data.Expr (Expr(..), (%))
import Data.Expr as Expr
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
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
import Type.Prelude (Proxy(..))
import Ui.Common (style, text)
import Ui.Console as Console
import Utility (todo)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

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
handleAction (EngineOutput_Action eo) = case eo of
  Output_EngineOutput o -> H.raise o # lift
  ExprQuery_EngineOutput q -> H.tell (Proxy @"Expr") unit q # lift
handleAction (ExprOutput_Action (is /\ eo)) = case eo of
  Output_ExprOutput o ->
    H.raise o # lift
  ExprInteraction pi -> do
    H.tell (Proxy @"Engine") unit (ExprInteraction_EngineQuery is pi) # lift
  PointInteraction_ExprOutput i pi ->
    H.tell (Proxy @"Engine") unit (PointInteraction_EngineQuery (Expr.Point is i) pi) # lift

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

data EngineQuery a
  = ExprInteraction_EngineQuery (List Int) ExprInteraction a
  | PointInteraction_EngineQuery Expr.Point PointInteraction a

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
    traceEngineM "Engine" (text $ "got mouse click from expr at " <> show (Array.fromFoldable is)) # lift
    -- deactivate previous handle points
    deactivateHandle =<< gets _.handle
    -- update handle
    case List.unsnoc is of
      Nothing -> pure unit
      Just { init, last } -> do
        let ix0@(Expr.Point is0 i0) = Expr.Point init last
        let ix1@(Expr.Point is1 i1) = Expr.Point init (last + 1)
        modify_ _ { handle = Expr.Cursor_Handle (Expr.Cursor ix0 ix1 Expr.Left_CursorFocus) }
        -- activate new handle points
        H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is0 $ PointQuery_ExprQuery i0 $ ModifyPointState (_ { style = CursorLeftPointStyle }) a') # lift
        H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is1 $ PointQuery_ExprQuery i1 $ ModifyPointState (_ { style = CursorRightPointStyle }) a') # lift
        pure unit
    pure a
handleEngineQuery (PointInteraction_EngineQuery ix@(Expr.Point is i) pi a) = case pi of
  Click_PointInteraction _event -> do
    when false do
      traceEngineM "Click" (text $ "got Click from Point at " <> show ix) # lift
      deactivateHandle =<< gets _.handle
      modify_ _ { handle = Expr.Cursor_Handle (Expr.Cursor ix ix Expr.Left_CursorFocus) }
      H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is $ PointQuery_ExprQuery i $ ModifyPointState (_ { style = PointCursorPointStyle }) a') # lift
    pure a
  StartDrag_PointInteraction _event -> do
    traceEngineM "Drag" (text $ "got StartDrag from Point at " <> show ix) # lift
    deactivateHandle =<< gets _.handle
    modify_ _
      { handle = Expr.Cursor_Handle (Expr.Cursor ix ix Expr.Left_CursorFocus)
      , drag_origin_handle = pure $ Expr.Cursor_Handle (Expr.Cursor ix ix Expr.Left_CursorFocus)
      }
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is $ PointQuery_ExprQuery i $ ModifyPointState (_ { style = PointCursorPointStyle }) a') # lift
    pure a
  MidDrag_PointInteraction _event -> do
    -- TODO
    pure a
  EndDrag_PointInteraction _event -> do
    traceEngineM "Drag" (text $ "got EndDrag from Point at " <> show ix) # lift
    modify_ _ { drag_origin_handle = Nothing }
    pure a

handleEngineAction :: EngineAction -> EngineM' Unit
handleEngineAction InitializeEngine = do
  traceEngineM "Editor . Engine" (text "initialized") # lift
handleEngineAction (ReceiveEngine input) = do
  traceEngineM "Editor . Engine" (text "received") # lift
  put $ initialEngineState input

activateHandle :: Expr.Handle -> EngineM' Unit
activateHandle handle = case handle of
  Expr.Cursor_Handle (Expr.Cursor (Expr.Point is0 i0) (Expr.Point is1 i1) _focus) -> do
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is0 $ PointQuery_ExprQuery i0 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is1 $ PointQuery_ExprQuery i1 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
  Expr.Select_Handle (Expr.Select (Expr.Point is00 i00) (Expr.Point is01 i01) (Expr.Point is10 i10) (Expr.Point is11 i11) _focus) -> do
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is00 $ PointQuery_ExprQuery i00 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is01 $ PointQuery_ExprQuery i01 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is10 $ PointQuery_ExprQuery i10 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is11 $ PointQuery_ExprQuery i11 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift

deactivateHandle :: Expr.Handle -> EngineM' Unit
deactivateHandle handle = case handle of
  Expr.Cursor_Handle (Expr.Cursor (Expr.Point is0 i0) (Expr.Point is1 i1) _focus) -> do
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is0 $ PointQuery_ExprQuery i0 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is1 $ PointQuery_ExprQuery i1 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
  Expr.Select_Handle (Expr.Select (Expr.Point is00 i00) (Expr.Point is01 i01) (Expr.Point is10 i10) (Expr.Point is11 i11) _focus) -> do
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is00 $ PointQuery_ExprQuery i00 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is01 $ PointQuery_ExprQuery i01 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is10 $ PointQuery_ExprQuery i10 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift
    H.raise (ExprQuery_EngineOutput \a' -> ExprQuery is11 $ PointQuery_ExprQuery i11 $ ModifyPointState (_ { style = NormalPointStyle }) a') # lift

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

data ExprQuery a = ExprQuery (List Int) (ExprQuery' a)
data ExprQuery' a
  = ModifyExprState (ExprState -> ExprState) a
  | PointQuery_ExprQuery Int (PointQuery a)

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
  | ExprOutput_ExprAction Int ExprOutput
  | ExprInteraction_ExprAction ExprInteraction
  | PointOutput_ExprAction Int PointOutput

-- | ClickExpr MouseEvent

type ExprSlots =
  ( "Expr" :: H.Slot ExprQuery ExprOutput Int
  , "Point" :: H.Slot PointQuery PointOutput Int
  )

type ExprOutput = List Int /\ ExprOutput'

data ExprOutput'
  = Output_ExprOutput Output
  | ExprInteraction ExprInteraction
  | PointInteraction_ExprOutput Int PointInteraction

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
      , HE.onClick (ClickExpr >>> ExprInteraction_ExprAction)
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
                    [ renderPoint i
                    , renderKid i e
                    ]
                , [ renderPoint (Array.length es) ]
                ]
          ]
      )

initialExprState :: ExprInput -> ExprState
initialExprState { expr } =
  { expr
  , ping: false
  }

handleExprQuery :: forall a. ExprQuery a -> ExprM' a
handleExprQuery (ExprQuery (i : is) q) = H.query (Proxy @"Expr") i (ExprQuery is q) # lift >>= case _ of
  Nothing -> throwError none
  Just a -> pure a
handleExprQuery (ExprQuery Nil q) = case q of
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
  H.raise ((i : is) /\ o) # lift
handleExprAction (ExprInteraction_ExprAction ei) = do
  case ei of
    ClickExpr event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (none /\ ExprInteraction ei) # lift
  pingExpr
-- Point stuff
handleExprAction (PointOutput_ExprAction i (Output_PointOutput o)) =
  H.raise (none /\ Output_ExprOutput o) # lift
handleExprAction (PointOutput_ExprAction i (PointInteraction pi)) = do
  case pi of
    Click_PointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    StartDrag_PointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    EndDrag_PointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    MidDrag_PointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (none /\ PointInteraction_ExprOutput i pi) # lift

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
  = NormalPointStyle
  | PointCursorPointStyle
  | CursorLeftPointStyle
  | CursorRightPointStyle

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
  = Click_PointInteraction MouseEvent
  | StartDrag_PointInteraction MouseEvent
  | EndDrag_PointInteraction MouseEvent
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
              NormalPointStyle -> []
              PointCursorPointStyle -> [ HH.ClassName "PointCursor" ]
              CursorLeftPointStyle -> [ HH.ClassName "CursorLeft" ]
              CursorRightPointStyle -> [ HH.ClassName "CursorRight" ]
          ]
      , HE.onClick (Click_PointInteraction >>> PointInteraction_PointAction)
      , HE.onMouseDown (StartDrag_PointInteraction >>> PointInteraction_PointAction)
      , HE.onMouseUp (EndDrag_PointInteraction >>> PointInteraction_PointAction)
      ]
      [ text " " ]

initialPointStyle :: PointInput -> PointState
initialPointStyle input =
  { style: NormalPointStyle
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
traceExprM label content = H.raise $ Tuple none $ Output_ExprOutput $ TellConsole \a -> Console.AddMessage { label, content } a

tracePointM :: String -> PlainHTML -> PointM Unit
tracePointM label content = H.raise $ Output_PointOutput $ TellConsole \a -> Console.AddMessage { label, content } a

