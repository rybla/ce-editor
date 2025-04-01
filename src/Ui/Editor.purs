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
import Data.Expr (Cursor(..), CursorFocus(..), Expr(..), Fragment(..), Handle(..), Index(..), Label(..), Path(..), Point(..), Step(..), getDragOrigin, getFragment, getHandleFromTo, getHandlePoints, getIndex, getIndicesAroundStep, getPath, insertAtPoint, mkCursorHandle, mkPointHandle, modifyDescendant_Span, toCursorHandle, toPointHandle, unZipper, unsnoc_Path, (%), (|:))
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
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
import Ui.Common (KeyInfo, classes, code, column, fromKeyboardEventToKeyInfo, list, style, text)
import Ui.Common as Ui
import Ui.Console as Console
import Utility (allEqual, forget, impossible, sortEquivalenceClasses)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.HTML.Window as Window
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
  , expr :: Expr
  }

data Action
  = Initialize
  | ViewExprOutput_Action ViewExprOutput
  | EngineOutput_Action EngineOutput
  | EngineQuery_Action (forall a. a -> EngineQuery a)

type Slots =
  ( "Engine" :: H.Slot EngineQuery EngineOutput Unit
  , "Expr" :: H.Slot ViewExprQuery ViewExprOutput Unit
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
    , expr: Root % editor.initial_exprs
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
    HH.div
      [ style do
          tell [ "flex-grow: 1", "flex-shrink: 1" ]
          tell [ "overflow: scroll" ]
          tell [ "padding: 0.5em" ]
      ]
      [ HH.slot (Proxy @"Engine") unit engine_component
          { editor: state.editor
          , expr: state.expr
          , handle: state.editor.initial_handle
          }
          EngineOutput_Action
      , HH.slot (Proxy @"Expr") unit viewExpr_component
          { expr: state.expr
          }
          ViewExprOutput_Action
      ]

handleAction :: Action -> M' Unit
handleAction Initialize = do
  lift $ trace "Editor" $ text "initialize"
  doc <- liftEffect $ HTML.Window.document =<< HTML.window
  lift $ H.subscribe' \_sub_id -> HQE.eventListener MouseEventType.mouseup (HTMLDocument.toEventTarget doc) $ MouseEvent.fromEvent >=> \_e -> do
    pure $ EngineQuery_Action EndDrag_EngineQuery
handleAction (EngineQuery_Action q) = do
  lift $ H.tell (Proxy @"Engine") unit q
handleAction (EngineOutput_Action eo) = case eo of
  Output_EngineOutput output -> H.raise output # lift
  ViewExprQuery_EngineOutput query -> H.tell (Proxy @"Expr") unit query # lift
  SetExpr_EngineOutput expr' -> modify_ _ { expr = expr' }
handleAction (ViewExprOutput_Action (is /\ eo)) = case eo of
  Output_ViewExprOutput o ->
    H.raise o # lift
  ExprInteraction pi -> do
    lift $ H.tell (Proxy @"Engine") unit (ExprInteraction_EngineQuery is pi)
  ViewPointInteraction_ViewExprOutput i pi ->
    lift $ H.tell (Proxy @"Engine") unit (ViewPointInteraction_EngineQuery (Point is i) pi)

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

data EngineQuery a
  = ExprInteraction_EngineQuery Path ExprInteraction a
  | ViewPointInteraction_EngineQuery Point ViewPointInteraction a
  | EndDrag_EngineQuery a

type EngineInput =
  { editor :: Editor
  , expr :: Expr
  , handle :: Handle
  }

type EngineState =
  { editor :: Editor
  , expr :: Expr
  , handle :: Handle
  , -- when dragging, this is the handle where the drag originated from
    drag_origin_handle :: Maybe Handle
  , clipboard :: Maybe Fragment
  }

data EngineAction
  = Initialize_EngineAction
  | Receive_EngineAction EngineInput
  | Keyboard_EngineAction KeyInfo

type EngineSlots = () :: Row Type

data EngineOutput
  = Output_EngineOutput Output
  | ViewExprQuery_EngineOutput (forall a. a -> ViewExprQuery a)
  | SetExpr_EngineOutput Expr

type EngineHTML = H.ComponentHTML EngineAction EngineSlots EngineM
type EngineM' = ExceptT PlainHTML EngineM
type EngineM = H.HalogenM EngineState EngineAction EngineSlots EngineOutput Aff

engine_component :: H.Component EngineQuery EngineInput EngineOutput Aff
engine_component = H.mkComponent { initialState: initialEngineState, eval, render }
  where
  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize_EngineAction
    , receive = pure <<< Receive_EngineAction
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
  Click_ViewExprAction _event -> do
    lift $ traceEngineM "Engine" $ text $ "got Click from Expr at " <> show "TODO: (Array.fromFoldable is)"
    case unsnoc_Path path of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        let { left: l, right: r } = getIndicesAroundStep last
        let h = mkCursorHandle (Cursor init l r Left_CursorFocus)
        setHandle h
    pure a
  StartDrag_ViewExprAction _event -> do
    case unsnoc_Path path of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        -- the point right before the expr
        let p = Point init (getIndicesAroundStep last).left
        lift $ traceEngineM "Editor . Drag" $ text $ "got StartDrag from Expr at " <> show p
        h <- gets _.handle
        let h' = getDragOrigin h p
        modify_ _ { drag_origin_handle = Just h' }
        setHandle h'
    pure a
  MidDrag_ViewExprAction _event -> do
    case unsnoc_Path path of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        -- the point right before the expr
        let p = Point init (getIndicesAroundStep last).left
        lift $ traceEngineM "Editor . Drag" $ text $ "got MidDrag from Expr at " <> show p
        updateDragToPoint p
    pure a
handleEngineQuery (ViewPointInteraction_EngineQuery p pi a) = case pi of
  StartDrag_ViewPointInteraction _event -> do
    lift $ traceEngineM "Editor . Drag" $ text $ "got StartDrag from Point at " <> show p
    h <- gets _.handle
    let h' = getDragOrigin h p
    modify_ _ { drag_origin_handle = Just h' }
    setHandle h'
    pure a
  MidDrag_ViewPointInteraction _event -> do
    lift $ traceEngineM "Editor . Drag" $ text $ "got MidDrag from Point at " <> show p
    updateDragToPoint p
    pure a
handleEngineQuery (EndDrag_EngineQuery a) = do
  lift $ traceEngineM "Editor . Drag" $ text $ "got EndDrag"
  modify_ _ { drag_origin_handle = Nothing }
  pure a

handleEngineAction :: EngineAction -> EngineM' Unit
handleEngineAction Initialize_EngineAction = do
  lift $ traceEngineM "Editor . Engine" $ text "initialize"
  doc <- liftEffect $ Window.document =<< HTML.window
  lift $ H.subscribe' \_subId ->
    HQE.eventListener
      KeyboardEvent.EventType.keydown
      (HTMLDocument.toEventTarget doc)
      (KeyboardEvent.fromEvent >>> map (fromKeyboardEventToKeyInfo >>> Keyboard_EngineAction))
  pure unit
handleEngineAction (Receive_EngineAction input) = do
  -- lift $ traceEngineM "Editor . Engine" $ text "receive"
  state <- get
  put $ (initialEngineState input)
    -- preserve old clipboard
    { clipboard = state.clipboard }
handleEngineAction (Keyboard_EngineAction ki) = do
  { handle, clipboard, expr } <- get
  case unit of
    -- copy fragment
    _ | ki.cmd && ki.key == "c" -> do
      let frag = getFragment handle expr
      lift $ traceEngineM "Editor . Keyboard" $ text $ "copy: " <> show frag
      modify_ _ { clipboard = pure $ frag }
    -- TODO: cut
    _ | ki.cmd && ki.key == "x" -> do
      let frag = getFragment handle expr
      lift $ traceEngineM "Editor . Keyboard" $ text $ "cut: " <> show frag
      -- getDesc
      modify_ _ { clipboard = pure $ frag }
    -- paste span
    _ | ki.cmd && ki.key == "v", Just (Span_Fragment span) <- clipboard, Just point <- toPointHandle handle -> do
      let expr' = insertAtPoint point span expr
      lift $ traceEngineM "Editor . Keyboard" $ list
        [ text "paste"
        , Ui.span [ text "expr  : ", code $ show expr ]
        , Ui.span [ text "span  : ", code $ show span ]
        , Ui.span [ text "expr' : ", code $ show expr' ]
        ]
      lift $ H.raise $ SetExpr_EngineOutput expr'
      setHandle $ mkPointHandle (Point (getPath point) (getIndex point + Index 1))
    -- paste zipper
    _ | ki.cmd && ki.key == "v", Just (Zipper_Fragment zip) <- clipboard, Just cursor <- toCursorHandle handle -> do
      let expr' = modifyDescendant_Span cursor (unZipper zip) expr
      lift $ traceEngineM "Editor . Keyboard" $ list
        [ text "paste"
        , Ui.span [ text "expr   : ", code $ show expr ]
        , Ui.span [ text "cursor : ", code $ show cursor ]
        , Ui.span [ text "zip    : ", code $ show zip ]
        , Ui.span [ text "expr'  : ", code $ show expr' ]
        ]
      lift $ H.raise $ SetExpr_EngineOutput expr'
      setHandle $ mkCursorHandle $ cursor
    _ -> pure unit
  pure unit

--------------------------------------------------------------------------------

updateDragToPoint :: Point -> EngineM' Unit
updateDragToPoint p = do
  e <- gets _.expr
  gets _.drag_origin_handle >>= case _ of
    Nothing -> pure unit
    Just drag_origin_handle -> case getHandleFromTo drag_origin_handle p e of
      Nothing -> do
        lift $ traceEngineM "Editor . Drag" $
          column
            [ text $ "updateDragToPoint (failure)"
            , list
                [ text $ "drag_origin_handle = " <> show drag_origin_handle
                , text $ "                 p = " <> show p
                ]
            ]
        pure unit
      Just h' -> do
        lift $ traceEngineM "Editor . Drag" $
          column
            [ text $ "updateDragToPoint (success)"
            , list
                [ text $ "drag_origin_handle = " <> show drag_origin_handle
                , text $ "                 p = " <> show p
                , text $ "getHandleFromTo ==> " <> show h'
                ]
            ]
        setHandle h'

setHandle :: Handle -> EngineM' Unit
setHandle h = do
  ps1 <- toggleHandleViewPointStyles false <$> gets _.handle
  modify_ _ { handle = h }
  let p_OL /\ p_IL /\ p_IR /\ p_OR = getHandlePoints h
  lift $ traceEngineM "Editor . Drag" $ list
    [ text $ "p_OL = " <> show p_OL
    , text $ "p_IL = " <> show p_IL
    , text $ "p_IR = " <> show p_IR
    , text $ "p_OR = " <> show p_OR
    ]
  let ps2 = toggleHandleViewPointStyles true h
  toggleViewPointStyles $ Map.unionWith forget ps1 ps2
  pure unit

toggleHandleViewPointStyles :: Boolean -> Handle -> Map Point ViewPointStyle
toggleHandleViewPointStyles active h@(Handle _ _ _ is_I _ _ _f) = toggleHandleViewPointStyles_helper active (is_I == Path Nil) p_OL p_IL p_IR p_OR
  where
  p_OL /\ p_IL /\ p_IR /\ p_OR = getHandlePoints h

toggleHandleViewPointStyles_helper :: Boolean -> Boolean -> Point -> Point -> Point -> Point -> Map Point ViewPointStyle
-- 
-- Point
toggleHandleViewPointStyles_helper active _inline p_OL_IL_IR_OR _p_IL _p_IR _p_OR | allEqual [ p_OL_IL_IR_OR, _p_IL, _p_IR, _p_OR ] = Map.fromFoldable [ p_OL_IL_IR_OR /\ toggleViewPointStyle active Cursor_Point_ViewPointStyle ]
-- 
-- Cursor
toggleHandleViewPointStyles_helper active _inline@true p_OL_IL_IR p_IL p_IR _p_OR | allEqual [ p_OL_IL_IR, p_IL, p_IR ] = Map.fromFoldable [ p_OL_IL_IR /\ toggleViewPointStyle active Cursor_Left_ViewPointStyle, _p_OR /\ toggleViewPointStyle active Cursor_Right_ViewPointStyle ]
toggleHandleViewPointStyles_helper active _inline@true p_OL p_IL_IR_OR _p_IR _p_OR | allEqual [ p_IL_IR_OR, _p_IR, _p_OR ] = Map.fromFoldable [ p_OL /\ toggleViewPointStyle active Cursor_Left_ViewPointStyle, p_IL_IR_OR /\ toggleViewPointStyle active Cursor_Right_ViewPointStyle ]
-- 
-- Select
--   - Select inline
toggleHandleViewPointStyles_helper active _inline@true p_OL p_IL_IR _p_IR p_OR | allEqual [ p_IL_IR, _p_IR ] = Map.fromFoldable [ p_OL /\ toggleViewPointStyle active Select_OuterLeft_ViewPointStyle, p_IL_IR /\ toggleViewPointStyle active Select_Inline_InnerLeft_And_InnerRight_ViewPointStyle, p_OR /\ toggleViewPointStyle active Select_OuterRight_ViewPointStyle ]
toggleHandleViewPointStyles_helper active _inline@true p_OL_IL _p_IL p_IR p_OR | allEqual [ p_OL_IL, _p_IL ] = Map.fromFoldable [ p_OL_IL /\ toggleViewPointStyle active Select_Inline_OuterLeft_And_InnerLeft_ViewPointStyle, p_IR /\ toggleViewPointStyle active Select_InnerRight_ViewPointStyle, p_OR /\ toggleViewPointStyle active Select_OuterRight_ViewPointStyle ]
toggleHandleViewPointStyles_helper active _inline@true p_OL p_IL p_IR_OR _p_OR | allEqual [ p_IR_OR, _p_OR ] = Map.fromFoldable [ p_OL /\ toggleViewPointStyle active Select_OuterLeft_ViewPointStyle, p_IL /\ toggleViewPointStyle active Select_InnerLeft_ViewPointStyle, p_IR_OR /\ toggleViewPointStyle active Select_Inline_InnerRight_And_OuterRight_ViewPointStyle ]
-- TODO: this is not a special case, right?
-- toggleHandleViewPointStyles_helper active inline p_OL p_IL p_IR p_OR | inline =  [ p_OL /\ toggleViewPointStyle active Select_OuterLeft_ViewPointStyle, p_IL /\ toggleViewPointStyle active Select_InnerLeft_ViewPointStyle, p_IR /\ toggleViewPointStyle active Select_InnerRight_ViewPointStyle, p_OR /\ toggleViewPointStyle active Select_OuterRight_ViewPointStyle ]
--   - Select not inline
toggleHandleViewPointStyles_helper active _inline p_OL_IL _p_IL p_IR p_OR | allEqual [ p_OL_IL, _p_IL ] = Map.fromFoldable [ p_OL_IL /\ toggleViewPointStyle active Select_OuterLeft_And_InnerLeft_ViewPointStyle, p_IR /\ toggleViewPointStyle active Select_InnerRight_ViewPointStyle, p_OR /\ toggleViewPointStyle active Select_OuterRight_ViewPointStyle ]
toggleHandleViewPointStyles_helper active _inline p_OL p_IL p_IR_OR _p_OR | allEqual [ p_IR_OR, _p_OR ] = Map.fromFoldable [ p_OL /\ toggleViewPointStyle active Select_OuterLeft_ViewPointStyle, p_IL /\ toggleViewPointStyle active Select_InnerLeft_ViewPointStyle, p_IR_OR /\ toggleViewPointStyle active Select_InnerRight_And_OuterRight_ViewPointStyle ]
toggleHandleViewPointStyles_helper active _inline p_OL p_IL_IR _p_IR p_OR | allEqual [ p_IL_IR, _p_IR ] = Map.fromFoldable [ p_OL /\ toggleViewPointStyle active Select_OuterLeft_ViewPointStyle, p_IL_IR /\ toggleViewPointStyle active Select_InnerLeft_And_InnerRight_ViewPointStyle, p_OR /\ toggleViewPointStyle active Select_OuterRight_ViewPointStyle ]
--   - Select normal
toggleHandleViewPointStyles_helper active _inline p_OL p_IL p_IR p_OR = Map.fromFoldable [ p_OL /\ toggleViewPointStyle active Select_OuterLeft_ViewPointStyle, p_IL /\ toggleViewPointStyle active Select_InnerLeft_ViewPointStyle, p_IR /\ toggleViewPointStyle active Select_InnerRight_ViewPointStyle, p_OR /\ toggleViewPointStyle active Select_OuterRight_ViewPointStyle ]

toggleViewPointStyles :: Map Point ViewPointStyle -> EngineM' Unit
toggleViewPointStyles xs =
  lift $ H.raise $
    ViewExprQuery_EngineOutput do
      ViewExprQuery $
        xs
          # Map.toUnfoldable
          # NonEmptyArray.fromArray
          # fromMaybe' impossible
          # map (\((Point is i) /\ s) -> SingleViewExprQuery is $ ViewPointQuery_ViewExprQuery i $ ModifyViewPointState (_ { style = s }) unit)

toggleViewPointStyle :: Boolean -> ViewPointStyle -> ViewPointStyle
toggleViewPointStyle false = const Plain_ViewPointStyle
toggleViewPointStyle true = identity

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

data ViewExprQuery a = ViewExprQuery (NonEmptyArray SingleViewExprQuery) a

data SingleViewExprQuery = SingleViewExprQuery Path ViewExprQuery'

data ViewExprQuery'
  = Modify_ViewExprQuery (ViewExprState -> ViewExprState)
  | ViewPointQuery_ViewExprQuery Index (ViewPointQuery Unit)

type ViewExprInput =
  { expr :: Expr
  }

type ViewExprState =
  { expr :: Expr
  , ping :: Boolean
  }

data ViewExprAction
  = Initialize_ViewExprAction
  | Receive_ViewExprAction ViewExprInput
  | ViewExprOutput_ViewExprAction Step ViewExprOutput
  | ExprInteraction_ViewExprAction ExprInteraction
  | ViewPointOutput_ViewExprAction Index ViewPointOutput

type ViewExprSlots =
  ( "Expr" :: H.Slot ViewExprQuery ViewExprOutput Step
  , "Point" :: H.Slot ViewPointQuery ViewPointOutput Index
  )

type ViewExprOutput = Path /\ ViewExprOutput'

data ViewExprOutput'
  = Output_ViewExprOutput Output
  | ExprInteraction ExprInteraction
  | ViewPointInteraction_ViewExprOutput Index ViewPointInteraction

data ExprInteraction
  = Click_ViewExprAction MouseEvent
  | StartDrag_ViewExprAction MouseEvent
  | MidDrag_ViewExprAction MouseEvent

type ViewExprHTML = H.ComponentHTML ViewExprAction ViewExprSlots ViewExprM
type ViewExprM' = ExceptT (Maybe PlainHTML) ViewExprM
type ViewExprM = H.HalogenM ViewExprState ViewExprAction ViewExprSlots ViewExprOutput Aff

viewExpr_component :: H.Component ViewExprQuery ViewExprInput ViewExprOutput Aff
viewExpr_component = H.mkComponent { initialState: initialViewExprState, eval, render }
  where
  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize_ViewExprAction
    , receive = pure <<< Receive_ViewExprAction
    , handleQuery = \query -> do
        state <- get
        handleViewExprQuery query # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewExprM "Editor . Expr . Error" err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleViewExprAction action # runExceptT >>= case _ of
          Left mb_err -> do
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewExprM "Editor . Expr . Error" err
            put state
          Right it -> pure it
    }

  render state =
    let
      Expr l es = state.expr
    in
      HH.div
        [ HP.classes $ [ [ HH.ClassName "Expr" ], if state.ping then [ H.ClassName "ping" ] else [] ] # fold
        -- , HE.onMouseDown (StartDrag_ViewExprAction >>> ExprInteraction_ViewExprAction)
        -- , HE.onMouseMove (MidDrag_ViewExprAction >>> ExprInteraction_ViewExprAction)
        ]
        ( fold
            [ [ HH.div [ HP.classes [ HH.ClassName "ExprLabel" ] ]
                  [ case l of
                      String s -> text s
                      Root -> text "root"
                  ]
              ]
            , let
                renderPoint i =
                  HH.slot (Proxy @"Point") i point_component
                    {}
                    (ViewPointOutput_ViewExprAction i)
                renderKid i e =
                  HH.slot (Proxy @"Expr") i viewExpr_component
                    { expr: e }
                    (ViewExprOutput_ViewExprAction i)
              in
                fold
                  [ es # foldMapWithIndex \i e ->
                      [ renderPoint (Index i)
                      , renderKid (Step i) e
                      ]
                  , [ renderPoint (Index (Array.length es)) ]
                  ]
            ]
        )

initialViewExprState :: ViewExprInput -> ViewExprState
initialViewExprState { expr } =
  { expr
  , ping: false
  }

handleViewExprQuery :: forall a. ViewExprQuery a -> ViewExprM' a
handleViewExprQuery (ViewExprQuery qs_ a) = do
  let qss = qs_ # NEArray.toArray # sortEquivalenceClasses \(SingleViewExprQuery p1 _) (SingleViewExprQuery p2 _) -> p1 == p2
  qss # traverse_ \qs -> do
    let SingleViewExprQuery p _ = qs # NEArray.head
    case p of
      Path Nil -> qs # traverse_ handleSingleViewExprQuery
      Path (i : is) -> do
        let qs' = ViewExprQuery (qs <#> \(SingleViewExprQuery _ q') -> (SingleViewExprQuery (Path is) q')) unit
        H.query (Proxy @"Expr") i qs' # lift >>= case _ of
          Nothing -> throwError none
          Just it -> pure it
  pure a

handleSingleViewExprQuery :: SingleViewExprQuery -> ViewExprM' Unit
handleSingleViewExprQuery (SingleViewExprQuery _ (Modify_ViewExprQuery f)) = do
  modify_ f
handleSingleViewExprQuery (SingleViewExprQuery _ (ViewPointQuery_ViewExprQuery i pq)) = do
  H.query (Proxy @"Point") i pq # lift >>= case _ of
    Nothing -> throwError none
    Just it -> pure it

handleViewExprAction :: ViewExprAction -> ViewExprM' Unit
handleViewExprAction Initialize_ViewExprAction = pure unit
handleViewExprAction (Receive_ViewExprAction input) = do
  state <- get
  let state' = initialViewExprState input
  when (state /= state') do
    -- lift $ traceViewExprM "Editor . ViewExpr" $ Ui.column
    --   [ Ui.text "receive"
    --   , Ui.list
    --       [ Ui.span [ text "expr' = ", code $ show state'.expr ]
    --       ]
    --   ]
    put state'
    -- ping_ViewExpr
    pure unit
-- kid Expr stuff
handleViewExprAction (ViewExprOutput_ViewExprAction i (is /\ o)) = do
  H.raise ((i |: is) /\ o) # lift
handleViewExprAction (ExprInteraction_ViewExprAction ei) = do
  case ei of
    Click_ViewExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    StartDrag_ViewExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    MidDrag_ViewExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (Path Nil /\ ExprInteraction ei) # lift
  -- ping_ViewExpr
  pure unit
-- Point stuff
handleViewExprAction (ViewPointOutput_ViewExprAction _i (Output_ViewPointOutput o)) =
  H.raise (Path Nil /\ Output_ViewExprOutput o) # lift
handleViewExprAction (ViewPointOutput_ViewExprAction i (ViewPointInteraction pi)) = do
  case pi of
    StartDrag_ViewPointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    MidDrag_ViewPointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (Path Nil /\ ViewPointInteraction_ViewExprOutput i pi) # lift

ping_ViewExpr :: ViewExprM' Unit
ping_ViewExpr = do
  modify_ _ { ping = true }
  Aff.delay (Milliseconds 500.0) # liftAff
  modify_ _ { ping = false }

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

data ViewPointQuery a = ModifyViewPointState (ViewPointState -> ViewPointState) a

type ViewPointInput =
  {}

type ViewPointState =
  { style :: ViewPointStyle
  }

data ViewPointStyle
  = Plain_ViewPointStyle
  | Cursor_Point_ViewPointStyle
  | Cursor_Left_ViewPointStyle
  | Cursor_Right_ViewPointStyle
  | Select_OuterLeft_ViewPointStyle
  | Select_InnerLeft_ViewPointStyle
  | Select_InnerRight_ViewPointStyle
  | Select_OuterRight_ViewPointStyle
  | Select_Inline_InnerLeft_And_InnerRight_ViewPointStyle
  | Select_Inline_OuterLeft_And_InnerLeft_ViewPointStyle
  | Select_Inline_InnerRight_And_OuterRight_ViewPointStyle
  | Select_OuterLeft_And_InnerLeft_ViewPointStyle
  | Select_InnerRight_And_OuterRight_ViewPointStyle
  | Select_InnerLeft_And_InnerRight_ViewPointStyle

derive instance Generic ViewPointStyle _

instance Eq ViewPointStyle where
  eq x = genericEq x

data ViewPointAction
  = Initialize_ViewPointAction
  | Receive_ViewPointAction ViewPointInput
  | ViewPointInteraction_ViewPointAction ViewPointInteraction

type ViewPointSlots = () :: Row Type

data ViewPointOutput
  = Output_ViewPointOutput Output
  | ViewPointInteraction ViewPointInteraction

data ViewPointInteraction
  = StartDrag_ViewPointInteraction MouseEvent
  | MidDrag_ViewPointInteraction MouseEvent

type ViewPointHTML = H.ComponentHTML ViewPointAction ViewPointSlots ViewPointM
type ViewPointM' = ExceptT (Maybe PlainHTML) ViewPointM
type ViewPointM = H.HalogenM ViewPointState ViewPointAction ViewPointSlots ViewPointOutput Aff

point_component :: H.Component ViewPointQuery ViewPointInput ViewPointOutput Aff
point_component = H.mkComponent { initialState, eval, render }
  where
  initialState = initialViewPointStyle

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize_ViewPointAction
    , receive = pure <<< Receive_ViewPointAction
    , handleQuery = \query -> do
        state <- get
        handleViewPointQuery query # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewPointM "Editor . Point . Error" err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleViewPointAction action # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewPointM "Editor . Point . Error" err
          Right it -> pure it
    }

  render state =
    HH.div
      [ classes $ fold
          [ [ "Point" ]
          , case state.style of
              Plain_ViewPointStyle -> []
              Cursor_Point_ViewPointStyle -> [ "Cursor_Point" ]
              Cursor_Left_ViewPointStyle -> [ "Cursor_Left" ]
              Cursor_Right_ViewPointStyle -> [ "Cursor_Right" ]
              Select_OuterRight_ViewPointStyle -> [ "Select_OuterRight" ]
              Select_InnerLeft_ViewPointStyle -> [ "Select_InnerLeft" ]
              Select_InnerRight_ViewPointStyle -> [ "Select_InnerRight" ]
              Select_OuterLeft_ViewPointStyle -> [ "Select_OuterLeft" ]
              Select_Inline_InnerLeft_And_InnerRight_ViewPointStyle -> [ "Select_Inline_InnerLeft_And_InnerRight" ]
              Select_Inline_OuterLeft_And_InnerLeft_ViewPointStyle -> [ "Select_Inline_OuterLeft_And_InnerLeft" ]
              Select_Inline_InnerRight_And_OuterRight_ViewPointStyle -> [ "Select_Inline_InnerRight_And_OuterRight" ]
              Select_OuterLeft_And_InnerLeft_ViewPointStyle -> [ "Select_OuterLeft_And_InnerLeft" ]
              Select_InnerRight_And_OuterRight_ViewPointStyle -> [ "Select_InnerRight_And_OuterRight" ]
              Select_InnerLeft_And_InnerRight_ViewPointStyle -> [ "Select_InnerLeft_And_InnerRight" ]
          ]
      , HE.onMouseDown (StartDrag_ViewPointInteraction >>> ViewPointInteraction_ViewPointAction)
      , HE.onMouseEnter (MidDrag_ViewPointInteraction >>> ViewPointInteraction_ViewPointAction)
      ]
      [ text " " ]

initialViewPointStyle :: ViewPointInput -> ViewPointState
initialViewPointStyle _input =
  { style: Plain_ViewPointStyle
  }

handleViewPointQuery :: forall a. ViewPointQuery a -> ViewPointM' a
handleViewPointQuery (ModifyViewPointState f a) = do
  modify_ f
  pure a

handleViewPointAction :: ViewPointAction -> ViewPointM' Unit
handleViewPointAction Initialize_ViewPointAction = do
  pure unit
handleViewPointAction (Receive_ViewPointAction input) = do
  state <- get
  let state' = initialViewPointStyle input
  when (state' /= state) do
    put state'
handleViewPointAction (ViewPointInteraction_ViewPointAction pi) = do
  H.raise (ViewPointInteraction pi) # lift

--------------------------------------------------------------------------------

trace :: String -> PlainHTML -> M Unit
trace label content = H.raise $ TellConsole \a -> Console.AddMessage { label, content } a

traceEngineM :: String -> PlainHTML -> EngineM Unit
traceEngineM label content = H.raise $ Output_EngineOutput $ TellConsole \a -> Console.AddMessage { label, content } a

traceViewExprM :: String -> PlainHTML -> ViewExprM Unit
traceViewExprM label content = H.raise $ Tuple (Path Nil) $ Output_ViewExprOutput $ TellConsole \a -> Console.AddMessage { label, content } a

traceViewPointM :: String -> PlainHTML -> ViewPointM Unit
traceViewPointM label content = H.raise $ Output_ViewPointOutput $ TellConsole \a -> Console.AddMessage { label, content } a

