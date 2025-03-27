module Ui.Editor where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (get, gets, modify_, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Expr (Expr(..), unsnoc_Path, (%), (|:))
import Data.Expr as Expr
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
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
import Ui.Common (KeyInfo, classes, column, fromKeyboardEventToKeyInfo, list, style, text)
import Ui.Console as Console
import Utility (allEqual, forget, impossible, sortEquivalenceClasses)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEvent.EventType
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
  , clipboard :: Maybe Clipboard
  }

data Clipboard
  = Expr_Clipboard Expr
  | Zipper_Clipboard Expr.Zipper

data EngineAction
  = InitializeEngine
  | ReceiveEngine EngineInput
  | Keyboard_EngineAction KeyInfo

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
  , clipboard: Nothing
  }

handleEngineQuery :: forall a. EngineQuery a -> EngineM' a
handleEngineQuery (ExprInteraction_EngineQuery path ei a) = case ei of
  Click_ExprAction _event -> do
    lift $ traceEngineM "Engine" $ text $ "got Click from Expr at " <> show "TODO: (Array.fromFoldable is)"
    case unsnoc_Path path of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        let { left: l, right: r } = Expr.getIndicesAroundStep last
        let h = Expr.mkCursorHandle (Expr.Cursor init l r Expr.Left_CursorFocus)
        setHandle h
    pure a
  StartDrag_ExprAction _event -> do
    case unsnoc_Path path of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        -- the point right before the expr
        let p = Expr.Point init (Expr.getIndicesAroundStep last).left
        lift $ traceEngineM "Drag" $ text $ "got StartDrag from Expr at " <> show p
        h <- gets _.handle
        let h' = Expr.getDragOrigin h p
        modify_ _ { drag_origin_handle = Just h' }
        setHandle h'
    pure a
  MidDrag_ExprAction _event -> do
    case unsnoc_Path path of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        -- the point right before the expr
        let p = Expr.Point init (Expr.getIndicesAroundStep last).left
        lift $ traceEngineM "Drag" $ text $ "got MidDrag from Expr at " <> show p
        updateDragToPoint p
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
  lift $ traceEngineM "Drag" $ text $ "got EndDrag"
  modify_ _ { drag_origin_handle = Nothing }
  pure a

handleEngineAction :: EngineAction -> EngineM' Unit
handleEngineAction InitializeEngine = do
  lift $ traceEngineM "Editor . Engine" $ text "initialized"
  doc <- liftEffect $ Window.document =<< HTML.window
  lift $ H.subscribe' \_subId ->
    HQE.eventListener
      KeyboardEvent.EventType.keydown
      (HTMLDocument.toEventTarget doc)
      (KeyboardEvent.fromEvent >>> map (fromKeyboardEventToKeyInfo >>> Keyboard_EngineAction))
  pure unit
handleEngineAction (ReceiveEngine input) = do
  lift $ traceEngineM "Editor . Engine" $ text "received"
  put $ initialEngineState input
handleEngineAction (Keyboard_EngineAction ki) = do
  lift $ traceEngineM "Keyboard" $ text $ show ki
  case unit of
    -- TODO: copy
    _ | ki.cmd && ki.key == "c" -> do
      pure unit
    -- TODO: cut
    _ | ki.cmd && ki.key == "x" -> pure unit
    -- TODO: paste
    _ | ki.cmd && ki.key == "v" -> pure unit
    _ -> pure unit
  pure unit

--------------------------------------------------------------------------------

updateDragToPoint :: Expr.Point -> EngineM' Unit
updateDragToPoint p = do
  e <- gets _.expr
  gets _.drag_origin_handle >>= case _ of
    Nothing -> pure unit
    Just drag_origin_handle -> case Expr.getHandleFromTo drag_origin_handle p e of
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
  ps1 <- toggleHandlePointStyles false <$> gets _.handle
  modify_ _ { handle = h }
  let p_OL /\ p_IL /\ p_IR /\ p_OR = Expr.getHandlePoints h
  lift $ traceEngineM "Drag" $ list
    [ text $ "p_OL = " <> show p_OL
    , text $ "p_IL = " <> show p_IL
    , text $ "p_IR = " <> show p_IR
    , text $ "p_OR = " <> show p_OR
    ]
  let ps2 = toggleHandlePointStyles true h
  togglePointStyles $ Map.unionWith forget ps1 ps2
  pure unit

toggleHandlePointStyles :: Boolean -> Expr.Handle -> Map Expr.Point PointStyle
toggleHandlePointStyles active h@(Expr.Handle _ _ _ is_I _ _ _f) = toggleHandlePointStyles_helper active (is_I == Expr.Path Nil) p_OL p_IL p_IR p_OR
  where
  p_OL /\ p_IL /\ p_IR /\ p_OR = Expr.getHandlePoints h

toggleHandlePointStyles_helper :: Boolean -> Boolean -> Expr.Point -> Expr.Point -> Expr.Point -> Expr.Point -> Map Expr.Point PointStyle
-- 
-- Point
toggleHandlePointStyles_helper active _inline p_OL_IL_IR_OR _p_IL _p_IR _p_OR | allEqual [ p_OL_IL_IR_OR, _p_IL, _p_IR, _p_OR ] = Map.fromFoldable [ p_OL_IL_IR_OR /\ togglePointStyle active Cursor_Point_PointStyle ]
-- 
-- Cursor
toggleHandlePointStyles_helper active _inline@true p_OL_IL_IR p_IL p_IR _p_OR | allEqual [ p_OL_IL_IR, p_IL, p_IR ] = Map.fromFoldable [ p_OL_IL_IR /\ togglePointStyle active Cursor_Left_PointStyle, _p_OR /\ togglePointStyle active Cursor_Right_PointStyle ]
toggleHandlePointStyles_helper active _inline@true p_OL p_IL_IR_OR _p_IR _p_OR | allEqual [ p_IL_IR_OR, _p_IR, _p_OR ] = Map.fromFoldable [ p_OL /\ togglePointStyle active Cursor_Left_PointStyle, p_IL_IR_OR /\ togglePointStyle active Cursor_Right_PointStyle ]
-- 
-- Select
--   - Select inline
toggleHandlePointStyles_helper active _inline@true p_OL p_IL_IR _p_IR p_OR | allEqual [ p_IL_IR, _p_IR ] = Map.fromFoldable [ p_OL /\ togglePointStyle active Select_OuterLeft_PointStyle, p_IL_IR /\ togglePointStyle active Select_Inline_InnerLeft_And_InnerRight_PointStyle, p_OR /\ togglePointStyle active Select_OuterRight_PointStyle ]
toggleHandlePointStyles_helper active _inline@true p_OL_IL _p_IL p_IR p_OR | allEqual [ p_OL_IL, _p_IL ] = Map.fromFoldable [ p_OL_IL /\ togglePointStyle active Select_Inline_OuterLeft_And_InnerLeft_PointStyle, p_IR /\ togglePointStyle active Select_InnerRight_PointStyle, p_OR /\ togglePointStyle active Select_OuterRight_PointStyle ]
toggleHandlePointStyles_helper active _inline@true p_OL p_IL p_IR_OR _p_OR | allEqual [ p_IR_OR, _p_OR ] = Map.fromFoldable [ p_OL /\ togglePointStyle active Select_OuterLeft_PointStyle, p_IL /\ togglePointStyle active Select_InnerLeft_PointStyle, p_IR_OR /\ togglePointStyle active Select_Inline_InnerRight_And_OuterRight_PointStyle ]
-- TODO: this is not a special case, right?
-- toggleHandlePointStyles_helper active inline p_OL p_IL p_IR p_OR | inline =  [ p_OL /\ togglePointStyle active Select_OuterLeft_PointStyle, p_IL /\ togglePointStyle active Select_InnerLeft_PointStyle, p_IR /\ togglePointStyle active Select_InnerRight_PointStyle, p_OR /\ togglePointStyle active Select_OuterRight_PointStyle ]
--   - Select not inline
toggleHandlePointStyles_helper active _inline p_OL_IL _p_IL p_IR p_OR | allEqual [ p_OL_IL, _p_IL ] = Map.fromFoldable [ p_OL_IL /\ togglePointStyle active Select_OuterLeft_And_InnerLeft_PointStyle, p_IR /\ togglePointStyle active Select_InnerRight_PointStyle, p_OR /\ togglePointStyle active Select_OuterRight_PointStyle ]
toggleHandlePointStyles_helper active _inline p_OL p_IL p_IR_OR _p_OR | allEqual [ p_IR_OR, _p_OR ] = Map.fromFoldable [ p_OL /\ togglePointStyle active Select_OuterLeft_PointStyle, p_IL /\ togglePointStyle active Select_InnerLeft_PointStyle, p_IR_OR /\ togglePointStyle active Select_InnerRight_And_OuterRight_PointStyle ]
toggleHandlePointStyles_helper active _inline p_OL p_IL_IR _p_IR p_OR | allEqual [ p_IL_IR, _p_IR ] = Map.fromFoldable [ p_OL /\ togglePointStyle active Select_OuterLeft_PointStyle, p_IL_IR /\ togglePointStyle active Select_InnerLeft_And_InnerRight_PointStyle, p_OR /\ togglePointStyle active Select_OuterRight_PointStyle ]
--   - Select normal
toggleHandlePointStyles_helper active _inline p_OL p_IL p_IR p_OR = Map.fromFoldable [ p_OL /\ togglePointStyle active Select_OuterLeft_PointStyle, p_IL /\ togglePointStyle active Select_InnerLeft_PointStyle, p_IR /\ togglePointStyle active Select_InnerRight_PointStyle, p_OR /\ togglePointStyle active Select_OuterRight_PointStyle ]

togglePointStyles :: Map Expr.Point PointStyle -> EngineM' Unit
togglePointStyles xs =
  lift $ H.raise $
    ExprQuery_EngineOutput do
      ExprQuery $
        xs
          # Map.toUnfoldable
          # NonEmptyArray.fromArray
          # fromMaybe' impossible
          # map (\((Expr.Point is i) /\ s) -> SingleExprQuery is $ PointQuery_ExprQuery i $ ModifyPointState (_ { style = s }) unit)

togglePointStyle :: Boolean -> PointStyle -> PointStyle
togglePointStyle false = const Plain_PointStyle
togglePointStyle true = identity

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

data ExprQuery a = ExprQuery (NonEmptyArray SingleExprQuery) a

data SingleExprQuery = SingleExprQuery Expr.Path ExprQuery'

data ExprQuery'
  = Modify_ExprQuery (ExprState -> ExprState)
  | PointQuery_ExprQuery Expr.Index (PointQuery Unit)

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

type ExprSlots =
  ( "Expr" :: H.Slot ExprQuery ExprOutput Expr.Step
  , "Point" :: H.Slot PointQuery PointOutput Expr.Index
  )

type ExprOutput = Expr.Path /\ ExprOutput'

data ExprOutput'
  = Output_ExprOutput Output
  | ExprInteraction ExprInteraction
  | PointInteraction_ExprOutput Expr.Index PointInteraction

data ExprInteraction
  = Click_ExprAction MouseEvent
  | StartDrag_ExprAction MouseEvent
  | MidDrag_ExprAction MouseEvent

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

  render { expr: Expr l es, ping } =
    HH.div
      [ HP.classes $ [ [ HH.ClassName "Expr" ], if ping then [ H.ClassName "ping" ] else [] ] # fold
      -- , HE.onMouseDown (StartDrag_ExprAction >>> ExprInteraction_ExprAction)
      -- , HE.onMouseMove (MidDrag_ExprAction >>> ExprInteraction_ExprAction)
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
handleExprQuery (ExprQuery qs_ a) = do
  let qss = qs_ # NEArray.toArray # sortEquivalenceClasses \(SingleExprQuery p1 _) (SingleExprQuery p2 _) -> p1 == p2
  qss # traverse_ \qs -> do
    let SingleExprQuery p _ = qs # NEArray.head
    case p of
      Expr.Path Nil -> qs # traverse_ handleSingleExprQuery
      Expr.Path (i : is) -> do
        let qs' = ExprQuery (qs <#> \(SingleExprQuery _ q') -> (SingleExprQuery (Expr.Path is) q')) unit
        H.query (Proxy @"Expr") i qs' # lift >>= case _ of
          Nothing -> throwError none
          Just it -> pure it
  pure a

handleSingleExprQuery :: SingleExprQuery -> ExprM' Unit
handleSingleExprQuery (SingleExprQuery _ (Modify_ExprQuery f)) = do
  modify_ f
handleSingleExprQuery (SingleExprQuery _ (PointQuery_ExprQuery i pq)) = do
  H.query (Proxy @"Point") i pq # lift >>= case _ of
    Nothing -> throwError none
    Just it -> pure it

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
    Click_ExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    StartDrag_ExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    MidDrag_ExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (Expr.Path Nil /\ ExprInteraction ei) # lift
  -- pingExpr
  pure unit
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
  = Plain_PointStyle
  | Cursor_Point_PointStyle
  | Cursor_Left_PointStyle
  | Cursor_Right_PointStyle
  | Select_OuterLeft_PointStyle
  | Select_InnerLeft_PointStyle
  | Select_InnerRight_PointStyle
  | Select_OuterRight_PointStyle
  | Select_Inline_InnerLeft_And_InnerRight_PointStyle
  | Select_Inline_OuterLeft_And_InnerLeft_PointStyle
  | Select_Inline_InnerRight_And_OuterRight_PointStyle
  | Select_OuterLeft_And_InnerLeft_PointStyle
  | Select_InnerRight_And_OuterRight_PointStyle
  | Select_InnerLeft_And_InnerRight_PointStyle

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
      [ classes $ fold
          [ [ "Point" ]
          , case state.style of
              Plain_PointStyle -> []
              Cursor_Point_PointStyle -> [ "Cursor_Point" ]
              Cursor_Left_PointStyle -> [ "Cursor_Left" ]
              Cursor_Right_PointStyle -> [ "Cursor_Right" ]
              Select_OuterRight_PointStyle -> [ "Select_OuterRight" ]
              Select_InnerLeft_PointStyle -> [ "Select_InnerLeft" ]
              Select_InnerRight_PointStyle -> [ "Select_InnerRight" ]
              Select_OuterLeft_PointStyle -> [ "Select_OuterLeft" ]
              Select_Inline_InnerLeft_And_InnerRight_PointStyle -> [ "Select_Inline_InnerLeft_And_InnerRight" ]
              Select_Inline_OuterLeft_And_InnerLeft_PointStyle -> [ "Select_Inline_OuterLeft_And_InnerLeft" ]
              Select_Inline_InnerRight_And_OuterRight_PointStyle -> [ "Select_Inline_InnerRight_And_OuterRight" ]
              Select_OuterLeft_And_InnerLeft_PointStyle -> [ "Select_OuterLeft_And_InnerLeft" ]
              Select_InnerRight_And_OuterRight_PointStyle -> [ "Select_InnerRight_And_OuterRight" ]
              Select_InnerLeft_And_InnerRight_PointStyle -> [ "Select_InnerLeft_And_InnerRight" ]
          ]
      , HE.onMouseDown (StartDrag_PointInteraction >>> PointInteraction_PointAction)
      , HE.onMouseEnter (MidDrag_PointInteraction >>> PointInteraction_PointAction)
      ]
      [ text " " ]

initialPointStyle :: PointInput -> PointState
initialPointStyle _input =
  { style: Plain_PointStyle
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

