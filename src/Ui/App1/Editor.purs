module Ui.App1.Editor where

import Prelude

import Control.Monad.State (get, modify)
import Data.Expr (Edit(..), Expr, Handle(..), Path, Point, SpanFocus(..), SpanH(..), ZipperFocus(..), getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeIndexes, getFocusPoint, normalizeHandle)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Move as Expr.Move
import Data.Expr.Render as Expr.Render
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor (Editor(..), runRenderM)
import Editor.Common (RenderM)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.Query.Event as HQE
import Type.Prelude (Proxy(..))
import Ui.App1.Common (BufferOutput(..), EditorAction(..), EditorHTML, EditorInput, EditorM, EditorOutput, EditorQuery, EditorSlots, EditorState, PointOutput(..), PointQuery(..), PointStatus(..), Snapshot)
import Ui.App1.Point as Point
import Ui.Event (fromEventToKeyInfo, matchKeyInfo, matchMapKeyInfo) as Event
import Ui.Halogen (classes)
import Utility (guardPure, isNonSpace, (:%=), (:=))
import Web.Event.Event (preventDefault) as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTML.HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEvent
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEventType

component :: forall l. Show l => H.Component EditorQuery (EditorInput l) EditorOutput Aff
component = H.mkComponent { initialState, eval, render }

--------------------------------------------------------------------------------
-- initialState
--------------------------------------------------------------------------------

initialState :: forall l. EditorInput l -> EditorState l
initialState _input@{ editor: Editor editor } =
  { editor: Editor editor
  , root: editor.initial_expr
  , initial_mb_handle
  , ref_mb_handle: unsafePerformEffect do Ref.new initial_mb_handle
  , ref_mb_dragOrigin: unsafePerformEffect do Ref.new none
  , clipboard: none
  , ref_history: unsafePerformEffect do Ref.new none
  , ref_future: unsafePerformEffect do Ref.new none
  }
  where
  initial_mb_handle = none

--------------------------------------------------------------------------------
-- eval
--------------------------------------------------------------------------------

eval :: forall l a. Show l => H.HalogenQ EditorQuery (EditorAction l) (EditorInput l) a -> H.HalogenM (EditorState l) (EditorAction l) (EditorSlots l) EditorOutput Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_EditorAction
  , handleAction = handleAction
  }

handleAction :: forall l. Show l => EditorAction l -> EditorM l Unit

handleAction Initialize_EditorAction = do
  -- Console.log "[Editor] initialize"
  doc <- liftEffect $ HTML.window >>= HTML.Window.document
  H.subscribe' \_subId -> HQE.eventListener MouseEventType.mouseup (doc # HTML.HTMLDocument.toEventTarget) $ pure <<< MouseUp_EditorAction
  H.subscribe' \_subId -> HQE.eventListener KeyboardEvent.keydown (doc # HTML.HTMLDocument.toEventTarget) $ pure <<< KeyDown_EditorAction
  handleAction Rerender_EditorAction

handleAction Rerender_EditorAction = do
  -- TODO: setHandle
  pure unit

handleAction (MouseUp_EditorAction _event) = do
  state <- get
  liftEffect do state.ref_mb_dragOrigin := none

handleAction (KeyDown_EditorAction event) = do
  state@{ editor: Editor editor } <- get
  mb_handle <- liftEffect $ Ref.read state.ref_mb_handle
  mb_dragOrigin <- liftEffect $ Ref.read state.ref_mb_dragOrigin
  bufferIsOpen <- case mb_handle of
    Nothing -> pure false
    Just handle -> isJust <<< join <$> H.request (Proxy @"Point") (handle # getFocusPoint) GetBufferInput_PointQuery
  let ki = Event.fromEventToKeyInfo event
  -- Console.logShow { ki }

  if bufferIsOpen then case unit of
    -- close buffer
    _ | ki # Event.matchKeyInfo (_ == "Escape") { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          H.tell (Proxy @"Point") (handle # getFocusPoint) $ SetBufferInput_PointQuery none
    _ -> pure unit
  else case unit of
    -- shortcut
    _ | Just handle <- mb_handle, Just bufferOption <- ki # editor.getShortcut state.root handle -> do
      liftEffect $ event # Event.preventDefault
      submitEdit bufferOption
    -- move
    _ | Just dir <- ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> do
          liftEffect $ state.ref_mb_dragOrigin := none
          setHandle $ pure editor.initial_handle
        Just handle -> do
          case
            Expr.Move.movePointUntil state.root dir (handle # getFocusPoint) \p ->
              guardPure (editor.isValidHandle state.root) (Point_Handle p)
            of
            Nothing -> do
              liftEffect $ state.ref_mb_dragOrigin := none
              setHandle (Just (Point_Handle (handle # getFocusPoint)))
            Just handle' -> do
              liftEffect $ state.ref_mb_dragOrigin := none
              setHandle (Just handle')
    -- drag move
    _ | Just dir <- ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure true, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> do
          -- initialize dragOrigin
          case mb_dragOrigin of
            Nothing -> do
              liftEffect $ state.ref_mb_dragOrigin := pure editor.initial_handle
            Just _ -> do
              pure unit
          setHandle $ pure editor.initial_handle
        Just handle -> do
          -- initialize dragOrigin
          dragOrigin <- case mb_dragOrigin of
            Nothing -> do
              liftEffect $ state.ref_mb_dragOrigin := pure handle
              pure handle
            Just dragOrigin -> do
              pure dragOrigin
          case
            Expr.Move.movePointUntil state.root dir (handle # getFocusPoint) \p ->
              state.root # Expr.Drag.drag dragOrigin p >>= guardPure (editor.isValidHandle state.root)
            of
            Nothing -> pure unit
            Just handle' -> do
              setHandle $ pure handle'
    -- move handle focus
    _ | Just dir <- ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure false, alt: pure true } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          liftEffect $ state.ref_mb_dragOrigin := none
          setHandle $ pure $ handle # Expr.Move.moveHandleFocus dir
    -- escape
    _ | ki # Event.matchKeyInfo (_ == "Escape") { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      liftEffect $ state.ref_mb_dragOrigin := none
      case mb_handle of
        Just h -> setHandle $ Expr.Move.escape h
        _ -> pure unit
    -- select all
    _ | ki # Event.matchKeyInfo (_ == "a") { cmd: pure true, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      liftEffect $ state.ref_mb_dragOrigin := none
      let j = state.root # getExtremeIndexes
      let h = normalizeHandle $ SpanH_Handle (SpanH { path: none, j_L: j._L, j_R: j._R }) Left_SpanFocus
      setHandle $ pure h
    -- copy
    _ | ki # Event.matchKeyInfo (_ == "c") { cmd: pure true, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Just handle -> do
          let _ /\ _ /\ frag = state.root # Expr.Edit.cut handle
          -- TODO: this should not snapshot
          modifyEditorState _
            { clipboard = pure frag
            }
        _ -> pure unit
    -- delete
    _ | ki # Event.matchKeyInfo (_ == "Backspace") { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Just handle -> do
          let root' /\ handle' /\ _ = state.root # Expr.Edit.cut handle
          modifyEditorState _
            { root = root'
            , initial_mb_handle = pure handle'
            }
        _ -> pure unit
    -- cut
    _ | ki # Event.matchKeyInfo (_ == "x") { cmd: pure true, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Just handle -> do
          let root' /\ handle' /\ frag = state.root # Expr.Edit.cut handle
          modifyEditorState _
            { root = root'
            , initial_mb_handle = pure handle'
            , clipboard = pure frag
            }
        _ -> pure unit
    -- paste
    _ | ki # Event.matchKeyInfo (_ == "v") { cmd: pure true, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle /\ state.clipboard of
        Just handle /\ Just clipboard -> do
          let root' /\ handle' = state.root # Expr.Edit.paste clipboard handle
          modifyEditorState _
            { root = root'
            , initial_mb_handle = pure handle'
            }
        _ -> pure unit
    -- redo
    _ | ki # Event.matchKeyInfo (_ == "z") { cmd: pure true, shift: pure true, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      redo
    -- undo
    _ | ki # Event.matchKeyInfo (_ == "z") { cmd: pure true, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      undo
    -- open buffer
    _ | ki # Event.matchKeyInfo (_ `Set.member` submit_keys) { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          let point = handle # getFocusPoint
          H.tell (Proxy @"Point") point $ SetBufferInput_PointQuery $ pure $ { editor: Editor editor, point, menu: editor.getEditMenu state.root handle, query: "" }
    _ | ki # Event.matchKeyInfo isNonSpace { cmd: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          let point = handle # getFocusPoint
          H.tell (Proxy @"Point") point $ SetBufferInput_PointQuery $ pure $ { editor: Editor editor, point, menu: editor.getEditMenu state.root handle, query: (unwrap ki).key }
    -- unrecognized keyboard event
    _ -> pure unit

handleAction (PointOutput_EditorAction (MouseDown_PointOutput _event p)) = do
  state@{ editor: Editor editor } <- get
  mb_handle <- liftEffect do Ref.read state.ref_mb_handle
  case mb_handle of
    Nothing -> do
      liftEffect do state.ref_mb_dragOrigin := pure (Point_Handle p)
      setHandle $ pure (Point_Handle p)
    Just h -> do
      when (editor.isValidHandle state.root h) do
        let dragOrigin = Expr.Drag.getDragOrigin h p
        liftEffect do state.ref_mb_dragOrigin := pure dragOrigin
        setHandle $ pure dragOrigin
handleAction (PointOutput_EditorAction (MouseEnter_PointOutput event p)) = do
  when (MouseEvent.buttons event == 1) do
    state@{ editor: Editor editor } <- get
    mb_dragOrigin <- liftEffect $ Ref.read state.ref_mb_dragOrigin
    case mb_dragOrigin of
      Nothing -> pure unit
      Just h -> do
        case state.root # Expr.Drag.drag h p of
          Nothing -> pure unit
          Just h' -> do
            when (editor.isValidHandle state.root h) do
              setHandle (pure h')
handleAction (PointOutput_EditorAction (BufferOutput_PointOutput (SubmitBuffer_BufferOutput bufferOption))) = do
  submitEdit bufferOption

submit_keys = Set.fromFoldable [ "Tab", " " ]

--------------------------------------------------------------------------------
-- undo and redo
--------------------------------------------------------------------------------

snapshot :: forall l. EditorM l Unit
snapshot = do
  state <- get
  mb_handle <- liftEffect $ state.ref_mb_handle # Ref.read
  liftEffect $ state.ref_history :%= ({ root: state.root, mb_handle } : _)
  liftEffect $ state.ref_future := none

undo :: forall l. EditorM l Unit
undo = do
  state <- get
  (liftEffect $ state.ref_history # Ref.read) >>= case _ of
    Nil -> pure unit
    s : history' -> do
      liftEffect $ state.ref_history := history'
      liftEffect $ state.ref_future :%= (s : _)
      loadSnapshot s

redo :: forall l. EditorM l Unit
redo = do
  state <- get
  (liftEffect $ state.ref_future # Ref.read) >>= case _ of
    Nil -> pure unit
    s : future' -> do
      liftEffect $ state.ref_history :%= (s : _)
      liftEffect $ state.ref_future := future'
      loadSnapshot s

loadSnapshot :: forall l. Snapshot l -> EditorM l Unit
loadSnapshot s = do
  setHandle' do
    get >>= \state -> liftEffect $ state.ref_mb_dragOrigin := none
    state <- modify _ { root = s.root, initial_mb_handle = s.mb_handle }
    pure state.initial_mb_handle

--------------------------------------------------------------------------------
-- modifyEditorState
--------------------------------------------------------------------------------

modifyEditorState :: forall l. (EditorState l -> EditorState l) -> EditorM l Unit
modifyEditorState f = do
  snapshot
  setHandle' do
    get >>= \state -> liftEffect $ state.ref_mb_dragOrigin := none
    state <- modify f
    pure state.initial_mb_handle

--------------------------------------------------------------------------------
-- submitEdit
--------------------------------------------------------------------------------

submitEdit :: forall l. Show l => Edit l -> EditorM l Unit
submitEdit (Fragment_Edit _frag lazy_result) = do
  let root' /\ handle' = lazy_result # Lazy.force
  modifyEditorState _
    { root = root'
    , initial_mb_handle = pure handle'
    }

--------------------------------------------------------------------------------
-- setHandle
--------------------------------------------------------------------------------

-- | turns off old handle, then turns on new handle
setHandle :: forall l. Maybe Handle -> EditorM l Unit
setHandle mb_handle = setHandle' $ pure mb_handle

-- | turns off old handle, then computes new handle, then turns on new handle.
-- | note that this DOES NOT reset ref_mb_dragOrigin.
setHandle' :: forall l. EditorM l (Maybe Handle) -> EditorM l Unit
setHandle' m_mb_handle = do
  state <- get
  mb_handle_old <- liftEffect $ Ref.read state.ref_mb_handle
  modifyHandle false mb_handle_old
  mb_handle_new <- m_mb_handle
  -- Console.log $ "[Editor] setHandle " <> show mb_handle_new
  modifyHandle true mb_handle_new
  liftEffect $ state.ref_mb_handle := mb_handle_new

modifyHandle :: forall l. Boolean -> Maybe Handle -> EditorM l Unit
modifyHandle b mb_handle = do
  let
    modifyClass :: Point -> Set PointStatus -> EditorM l Unit
    modifyClass p ss' =
      if b then
        H.tell (Proxy @"Point") p $ ModifyStatuses_PointQuery (_ `Set.union` ss')
      else
        H.tell (Proxy @"Point") p $ ModifyStatuses_PointQuery (_ `Set.difference` ss')
  case mb_handle of
    Nothing -> pure unit
    Just (Point_Handle p) -> do
      modifyClass p ss_Point_Handle
    Just (SpanH_Handle h f) -> do
      let p = getEndPoints_SpanH h
      case f of
        Left_SpanFocus -> do
          modifyClass p._L ss_SpanH_Handle_Left_Focus
          modifyClass p._R ss_SpanH_Handle_Right
        Right_SpanFocus -> do
          modifyClass p._L ss_SpanH_Handle_Left
          modifyClass p._R ss_SpanH_Handle_Right_Focus
    Just (ZipperH_Handle h f) -> do
      let p = getEndPoints_ZipperH h
      case f of
        OuterLeft_ZipperFocus -> do
          modifyClass p._OL ss_ZipperH_Handle_OuterLeft_Focus
          modifyClass p._IL ss_ZipperH_Handle_InnerLeft
          modifyClass p._IR ss_ZipperH_Handle_InnerRight
          modifyClass p._OR ss_ZipperH_Handle_OuterRight
        InnerLeft_ZipperFocus -> do
          modifyClass p._OL ss_ZipperH_Handle_OuterLeft
          modifyClass p._IL ss_ZipperH_Handle_InnerLeft_Focus
          modifyClass p._IR ss_ZipperH_Handle_InnerRight
          modifyClass p._OR ss_ZipperH_Handle_OuterRight
        InnerRight_ZipperFocus -> do
          modifyClass p._OL ss_ZipperH_Handle_OuterLeft
          modifyClass p._IL ss_ZipperH_Handle_InnerLeft
          modifyClass p._IR ss_ZipperH_Handle_InnerRight_Focus
          modifyClass p._OR ss_ZipperH_Handle_OuterRight
        OuterRight_ZipperFocus -> do
          modifyClass p._OL ss_ZipperH_Handle_OuterLeft
          modifyClass p._IL ss_ZipperH_Handle_InnerLeft
          modifyClass p._IR ss_ZipperH_Handle_InnerRight
          modifyClass p._OR ss_ZipperH_Handle_OuterRight_Focus

-- Point_Handle

ss_Point_Handle = Set.fromFoldable [ Point_Handle_PointStatus ]

-- SpanH_Handle

ss_SpanH_Handle_Left = Set.fromFoldable [ SpanH_Handle_Left_PointStatus ]
ss_SpanH_Handle_Right = Set.fromFoldable [ SpanH_Handle_Right_PointStatus ]

ss_SpanH_Handle_Left_Focus = Set.fromFoldable [ SpanH_Handle_Left_PointStatus, LeftFocus_PointStatus ]
ss_SpanH_Handle_Right_Focus = Set.fromFoldable [ SpanH_Handle_Right_PointStatus, RightFocus_PointStatus ]

-- ZipperH_Handle

ss_ZipperH_Handle_OuterLeft = Set.fromFoldable [ ZipperH_Handle_OuterLeft_PointStatus ]
ss_ZipperH_Handle_InnerLeft = Set.fromFoldable [ ZipperH_Handle_InnerLeft_PointStatus ]
ss_ZipperH_Handle_InnerRight = Set.fromFoldable [ ZipperH_Handle_InnerRight_PointStatus ]
ss_ZipperH_Handle_OuterRight = Set.fromFoldable [ ZipperH_Handle_OuterRight_PointStatus ]

ss_ZipperH_Handle_OuterLeft_Focus = Set.fromFoldable [ ZipperH_Handle_OuterLeft_PointStatus, LeftFocus_PointStatus ]
ss_ZipperH_Handle_InnerLeft_Focus = Set.fromFoldable [ ZipperH_Handle_InnerLeft_PointStatus, LeftFocus_PointStatus ]
ss_ZipperH_Handle_InnerRight_Focus = Set.fromFoldable [ ZipperH_Handle_InnerRight_PointStatus, RightFocus_PointStatus ]
ss_ZipperH_Handle_OuterRight_Focus = Set.fromFoldable [ ZipperH_Handle_OuterRight_PointStatus, RightFocus_PointStatus ]

--------------------------------------------------------------------------------
-- render
--------------------------------------------------------------------------------

render :: forall l. Show l => EditorState l -> EditorHTML l
render state =
  HHK.div [ classes [ "Editor" ] ]
    [ "root" /\
        HHK.div [ classes [ "root" ] ]
          [ "0" /\
              HH.div [ classes [ "Expr" ] ]
                ( state.root
                    # renderExpr state Nil
                    # runRenderM
                )
          ]
    ]

renderExpr :: forall l. Show l => EditorState l -> Path -> Expr l -> RenderM (Array (EditorHTML l))
renderExpr state@{ editor: Editor editor } path expr = do
  Expr.Render.renderExpr
    { indentLevel: 1
    , render_kid: renderExpr state
    , render_point: renderPoint state
    , assembleExpr: editor.assembleExpr
    }
    path
    expr

renderPoint :: forall l. Show l => EditorState l -> Point -> EditorHTML l
renderPoint state point =
  HH.slot (Proxy @"Point") point Point.component { editor: state.editor, point } PointOutput_EditorAction

