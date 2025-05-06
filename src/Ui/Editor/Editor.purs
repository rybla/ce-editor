module Ui.Editor.Editor where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (get, modify, put)
import Control.Monad.Writer (runWriter)
import Data.Array as Array
import Data.Expr (Edit, EditAt, Expr, Fragment(..), Handle(..), Path, Point, Span(..), SpanFocus(..), SpanH(..), ZipperFocus(..), applyEdit, getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeIndexes, getFocusPoint, normalizeHandle)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Move as Expr.Move
import Data.Expr.Render as Expr.Render
import Data.Foldable (fold)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor (Editor(..), Label, runRenderM)
import Editor.Common (RenderM)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.Event as HQE
import Type.Prelude (Proxy(..))
import Ui.Browser (navigator_clibpoard_writeText)
import Ui.Editor.Common (BufferOutput(..), EditorAction(..), EditorHTML, EditorInput, EditorM, EditorOutput, EditorQuery, EditorSlots, EditorState, PointOutput(..), PointQuery(..), PointStatus(..), Snapshot, toPureEditorState)
import Ui.Editor.Config as Config
import Ui.Editor.Point as Point
import Ui.Event (alt, cmd, keyEq, keyMember, keyRegex, not_alt, not_cmd, not_shift, shift)
import Ui.Event (fromEventToKeyInfo, matchKeyInfoPattern') as Event
import Ui.Halogen (classes)
import Utility (guardPure, isNonSpace_regex, (:%=), (:=))
import Web.Event.Event (preventDefault) as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTML.HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEvent
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEventType

component :: forall c. Show c => H.Component EditorQuery (EditorInput c) EditorOutput Aff
component = H.mkComponent { initialState, eval, render }

--------------------------------------------------------------------------------
-- initialState
--------------------------------------------------------------------------------

initialState :: forall c. EditorInput c -> EditorState c
initialState _input@{ editor: Editor editor } =
  { editor: Editor editor
  , root: editor.initialExpr
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

eval :: forall c a. Show c => H.HalogenQ EditorQuery (EditorAction c) (EditorInput c) a -> H.HalogenM (EditorState c) (EditorAction c) (EditorSlots c) EditorOutput Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_EditorAction
  , receive = pure <<< Receive_EditorAction
  , handleAction = handleAction
  }

handleAction :: forall c. Show c => EditorAction c -> EditorM c Unit

handleAction Initialize_EditorAction = do
  -- Console.log "[Editor] initialize"
  doc <- liftEffect $ HTML.window >>= HTML.Window.document
  H.subscribe' \_subId -> HQE.eventListener MouseEventType.mouseup (doc # HTML.HTMLDocument.toEventTarget) $ pure <<< MouseUp_EditorAction
  H.subscribe' \_subId -> HQE.eventListener KeyboardEvent.keydown (doc # HTML.HTMLDocument.toEventTarget) $ pure <<< KeyDown_EditorAction
  handleAction Rerender_EditorAction

handleAction (Receive_EditorAction input) = do
  put $ initialState input
  handleAction Rerender_EditorAction

handleAction Rerender_EditorAction = do
  -- TODO: setHandle
  pure unit

handleAction (MouseUp_EditorAction _event) = do
  state <- get
  liftEffect do state.ref_mb_dragOrigin := none

handleAction (KeyDown_EditorAction event) = do
  state@{ editor: Editor editor } <- get
  purestate <- state # toPureEditorState # liftEffect
  mb_handle <- liftEffect $ Ref.read state.ref_mb_handle
  mb_dragOrigin <- liftEffect $ Ref.read state.ref_mb_dragOrigin
  bufferIsOpen <- case mb_handle of
    Nothing -> pure false
    Just handle -> isJust <<< join <$> H.request (Proxy @"Point") (handle # getFocusPoint) GetBufferInput_PointQuery
  let ki = Event.fromEventToKeyInfo event
  -- Console.logShow { ki }

  if bufferIsOpen then case unit of
    -- close buffer
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "Escape", not_cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          H.tell (Proxy @"Point") (handle # getFocusPoint) $ SetBufferInput_PointQuery none
    _ -> pure unit
  else case unit of
    -- shortcut
    _ | Just edit /\ _diagnostics <- editor.getShortcut ki purestate # runMaybeT # runWriter -> do
      liftEffect $ event # Event.preventDefault
      -- TODO: report diagnostics
      submitEdit edit
    -- move
    _ | Just dir <- ki # Expr.Move.fromKeyInfoToMoveDir -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> do
          liftEffect $ state.ref_mb_dragOrigin := none
          setHandle $ pure editor.initialHandle
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
    _ | Just dir <- Expr.Move.fromKeyInfoToDragMoveDir ki -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> do
          -- initialize dragOrigin
          case mb_dragOrigin of
            Nothing -> do
              liftEffect $ state.ref_mb_dragOrigin := pure editor.initialHandle
            Just _ -> do
              pure unit
          setHandle $ pure editor.initialHandle
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
    _ | Just cycle <- ki # Expr.Move.fromKeyInfoToCycle -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          liftEffect $ state.ref_mb_dragOrigin := none
          setHandle $ pure $ handle # Expr.Move.cycleHandleFocus cycle
    -- escape
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "Escape", not_cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      liftEffect $ state.ref_mb_dragOrigin := none
      case mb_handle of
        Just h -> setHandle $ Expr.Move.escape h
        _ -> pure unit
    -- select all
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "a", cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      liftEffect $ state.ref_mb_dragOrigin := none
      let j = state.root # getExtremeIndexes
      let h = normalizeHandle $ SpanH_Handle (SpanH { path: none, j_L: j._L, j_R: j._R }) Left_SpanFocus
      setHandle $ pure h
    -- copy
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "c", cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      submitEditAt Expr.Edit.copy
      state' <- get
      case state'.clipboard of
        Just (Span_Fragment (Span es)) -> do
          let Editor editor = state'.editor
          liftEffect $ navigator_clibpoard_writeText $ String.joinWith "\n" $ map editor.printExpr es
        _ -> pure unit
    -- delete
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "Backspace", not_cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      submitEditAt $ Expr.Edit.delete' { isValidHandle: editor.isValidHandle }
    -- delete sibling
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "Backspace", not_cmd, not_shift, alt ] -> do
      liftEffect $ event # Event.preventDefault
      submitEditAt $ Expr.Edit.delete'_sibling { isValidHandle: editor.isValidHandle }
    -- cut
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "x", cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      submitEditAt Expr.Edit.cut
    -- paste
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "v", cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      submitEditAt Expr.Edit.paste
    -- redo
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "z", cmd, shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      redo
    -- undo
    _ | ki # Event.matchKeyInfoPattern' [ keyEq "z", cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      undo
    -- open buffer
    _ | ki # Event.matchKeyInfoPattern' [ keyMember openBuffer_keys, not_cmd, not_shift, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          let point = handle # getFocusPoint
          H.tell (Proxy @"Point") point $ SetBufferInput_PointQuery $ pure $ { editor: Editor editor, point, menu: editor.getEditMenu purestate, query: "" }
    _ | ki # Event.matchKeyInfoPattern' [ keyRegex isNonSpace_regex, not_cmd, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          let point = handle # getFocusPoint
          H.tell (Proxy @"Point") point $ SetBufferInput_PointQuery $ pure $ { editor: Editor editor, point, menu: editor.getEditMenu purestate, query: (unwrap ki).key }
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
handleAction (PointOutput_EditorAction (BufferOutput_PointOutput (SubmitBuffer_BufferOutput edit))) = do
  submitEdit edit

openBuffer_keys = Set.fromFoldable [ "Tab" ]

--------------------------------------------------------------------------------
-- undo and redo
--------------------------------------------------------------------------------

getSnapshot :: forall c. EditorM c (Snapshot c)
getSnapshot = do
  state <- get
  mb_handle <- liftEffect $ state.ref_mb_handle # Ref.read
  pure { root: state.root, mb_handle }

saveSnapshot :: forall c. Show c => EditorM c Unit
saveSnapshot = do
  state <- get
  s <- getSnapshot
  when Config.log_undo_and_redo do
    Console.log $ "[saveSnapshot] " <> show s
  liftEffect $ state.ref_history :%= (s : _)
  liftEffect $ state.ref_future := none

undo :: forall c. Show c => EditorM c Unit
undo = do
  state <- get
  when Config.log_undo_and_redo do
    Console.log $ "[undo] " <> show state.root
  (liftEffect $ state.ref_history # Ref.read) >>= case _ of
    Nil -> pure unit
    s' : history' -> do
      s <- getSnapshot
      liftEffect $ state.ref_history := history'
      liftEffect $ state.ref_future :%= (s : _)
      loadSnapshot s'

redo :: forall c. Show c => EditorM c Unit
redo = do
  state <- get
  when Config.log_undo_and_redo do
    Console.log $ "[redo] " <> show state.root
  (liftEffect $ state.ref_future # Ref.read) >>= case _ of
    Nil -> pure unit
    s' : future' -> do
      s <- getSnapshot
      liftEffect $ state.ref_history :%= (s : _)
      liftEffect $ state.ref_future := future'
      loadSnapshot s'

loadSnapshot :: forall c. Snapshot c -> EditorM c Unit
loadSnapshot s = do
  setHandle' do
    get >>= \state -> liftEffect $ state.ref_mb_dragOrigin := none
    state <- modify _ { root = s.root, initial_mb_handle = s.mb_handle }
    pure state.initial_mb_handle

--------------------------------------------------------------------------------
-- submitEdit
--------------------------------------------------------------------------------

submitEditAt :: forall c. Show c => EditAt (Label c ()) -> EditorM c Unit
submitEditAt editAt = do
  state <- get >>= (liftEffect <<< toPureEditorState)
  let mb_edit /\ _diagnostics = state # editAt # runMaybeT # runWriter
  -- TODO: report diagnostics
  case mb_edit of
    Nothing -> pure unit
    Just edit -> submitEdit edit

submitEdit :: forall c. Show c => Edit (Label c ()) -> EditorM c Unit
submitEdit edit = do
  input <- get >>= (liftEffect <<< toPureEditorState)

  let mb_output /\ _diagnostics = applyEdit edit input # runMaybeT # runWriter
  case mb_output of
    Nothing -> do
      when Config.log_edits do
        Console.log $ String.joinWith "\n"
          [ Array.replicate 10 "====" # fold
          , "[log_edits]"
          , ""
          , "edit:"
          , show edit
          , "input state:"
          , "{ root: " <> show input.root <> "\n, mb_handle: " <> show input.mb_handle <> "\n, clipboard: " <> show input.clipboard <> "\n}"
          , "output state:"
          , "{{failed}}"
          , Array.replicate 10 "====" # fold
          ]
      -- TODO: report diagnostics
      pure unit
    Just output -> do
      when Config.log_edits do
        Console.log $ String.joinWith "\n"
          [ Array.replicate 10 "====" # fold
          , "[log_edits]"
          , ""
          , "edit:"
          , show edit
          , "input state:"
          , "{ root: " <> show input.root <> "\n, mb_handle: " <> show input.mb_handle <> "\n, clipboard: " <> show input.clipboard <> "\n}"
          , "output state:"
          , "{ root: " <> show output.root <> "\n, mb_handle: " <> show output.mb_handle <> "\n, clipboard: " <> show output.clipboard <> "\n}"
          , Array.replicate 10 "====" # fold
          , Array.replicate 10 "====" # fold
          ]
      modifyEditorState _
        { root = output.root
        , initial_mb_handle = output.mb_handle
        , clipboard = output.clipboard
        }

--------------------------------------------------------------------------------
-- modifyEditorState
--------------------------------------------------------------------------------

modifyEditorState :: forall c. Show c => (EditorState c -> EditorState c) -> EditorM c Unit
modifyEditorState f = do
  saveSnapshot
  setHandle' do
    get >>= \state -> liftEffect $ state.ref_mb_dragOrigin := none
    state <- modify f
    pure state.initial_mb_handle

--------------------------------------------------------------------------------
-- setHandle
--------------------------------------------------------------------------------

-- | turns off old handle, then turns on new handle
setHandle :: forall c. Maybe Handle -> EditorM c Unit
setHandle mb_handle = setHandle' $ pure mb_handle

-- | turns off old handle, then computes new handle, then turns on new handle.
-- | note that this DOES NOT reset ref_mb_dragOrigin.
setHandle' :: forall c. EditorM c (Maybe Handle) -> EditorM c Unit
setHandle' m_mb_handle = do
  state <- get
  mb_handle_old <- liftEffect $ Ref.read state.ref_mb_handle
  -- Console.log $ "[Editor.setHandle'] mb_handle_old = " <> show mb_handle_old
  modifyHandle false mb_handle_old
  mb_handle_new <- m_mb_handle
  -- Console.log $ "[Editor.setHandle'] mb_handle_new = " <> show mb_handle_new
  modifyHandle true mb_handle_new
  liftEffect $ state.ref_mb_handle := mb_handle_new

modifyHandle :: forall c. Boolean -> Maybe Handle -> EditorM c Unit
modifyHandle b mb_handle = do
  let
    modifyClass :: Point -> Set PointStatus -> EditorM c Unit
    modifyClass p ss' =
      if b then do
        -- H.tell (Proxy @"Point") p $ ModifyStatuses_PointQuery (_ `Set.union` ss')
        success <- map isJust $ H.request (Proxy @"Point") p $ const $ ModifyStatuses_PointQuery (_ `Set.union` ss') unit
        unless success do
          Console.log $ "[Editor.modifyHandle] failed to request Point " <> show p
      else do
        -- H.tell (Proxy @"Point") p $ ModifyStatuses_PointQuery (_ `Set.difference` ss')
        success <- map isJust $ H.request (Proxy @"Point") p $ const $ ModifyStatuses_PointQuery (_ `Set.difference` ss') unit
        unless success do
          Console.log $ "[Editor.modifyHandle] failed to request Point " <> show p
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

render :: forall c. Show c => EditorState c -> EditorHTML c
render state =
  HH.div [ classes [ "Editor" ] ]
    [ HH.div [ classes [ "root" ] ]
        [ HH.div [ classes [ "Expr" ] ]
            ( state.root
                # renderExpr state Nil
                # runRenderM
            )
        ]
    ]

renderExpr :: forall c. Show c => EditorState c -> Path -> Expr (Label c ()) -> RenderM (Array (EditorHTML c))
renderExpr state@{ editor: Editor editor } path expr = do
  Expr.Render.renderExpr
    { renderKid: renderExpr state
    , renderPoint: renderPoint state
    , assembleExpr: editor.assembleExpr
    }
    path
    expr

renderPoint :: forall c. Show c => EditorState c -> Point -> EditorHTML c
renderPoint state point =
  HH.slot (Proxy @"Point") point Point.component { editor: state.editor, point } PointOutput_EditorAction

