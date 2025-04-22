module Ui.App1.Editor where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (get, modify)
import Data.Array as Array
import Data.Expr (BufferOption(..), Expr(..), Fragment(..), Handle(..), Path, Point(..), SpanFocus(..), ZipperFocus(..), getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeIndexes, getFocusPoint, getIndexesAroundStep, traverseStepsAndKids)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Move as Expr.Move
import Data.Foldable (and)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor.Example.Editor2 (L)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Exception (throw)
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
import Utility (fromMaybeM, isAlpha, todo, (:%=), (:=))
import Web.Event.Event (preventDefault) as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTML.HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEvent
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEventType

component :: H.Component EditorQuery EditorInput EditorOutput Aff
component = H.mkComponent { initialState, eval, render }

--------------------------------------------------------------------------------
-- initialState
--------------------------------------------------------------------------------

initialState :: EditorInput -> EditorState
initialState input =
  { editor: input.editor
  , root: input.editor.initial_expr
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

eval :: forall a. H.HalogenQ EditorQuery EditorAction EditorInput a -> H.HalogenM EditorState EditorAction EditorSlots EditorOutput Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_EditorAction
  , handleAction = handleAction
  }

handleAction :: EditorAction -> EditorM Unit

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
  state <- get
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
    -- move
    _ | Just dir <- ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      liftEffect $ state.ref_mb_dragOrigin := none
      case mb_handle of
        Nothing -> do
          setHandle $ pure state.editor.initial_handle
        Just handle -> do
          case Expr.Move.movePoint state.root dir (handle # getFocusPoint) of
            Nothing -> setHandle (Just (Point_Handle (handle # getFocusPoint)))
            Just point -> setHandle (Just (Point_Handle point))
    -- drag move
    _ | Just dir <- ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure true, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> do
          -- initialize dragOrigin
          case mb_dragOrigin of
            Nothing -> do
              liftEffect $ state.ref_mb_dragOrigin := pure state.editor.initial_handle
            Just _ -> do
              pure unit
          setHandle $ pure state.editor.initial_handle
        Just handle -> do
          -- initialize dragOrigin
          dragOrigin <- case mb_dragOrigin of
            Nothing -> do
              liftEffect $ state.ref_mb_dragOrigin := pure handle
              pure handle
            Just dragOrigin -> do
              pure dragOrigin
          case Expr.Move.movePointUntil state.root dir (handle # getFocusPoint) \p -> state.root # Expr.Drag.drag dragOrigin p of
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
    -- copy
    _ | ki # Event.matchKeyInfo (_ == "c") { cmd: pure true, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Just handle -> do
          let root' /\ _ /\ frag = state.root # Expr.Edit.cut handle
          modifyEditorState _
            { root = root'
            , clipboard = pure frag
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
    _ | ki # Event.matchKeyInfo (_ == "Enter") { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          let point = handle # getFocusPoint
          H.tell (Proxy @"Point") point $ SetBufferInput_PointQuery $ pure $ { options: state.editor.bufferOptions_point point, query: "" }
    _ | ki # Event.matchKeyInfo isAlpha { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          let point = handle # getFocusPoint
          H.tell (Proxy @"Point") point $ SetBufferInput_PointQuery $ pure $ { options: state.editor.bufferOptions_point point, query: (unwrap ki).key }
    -- unrecognized keyboard event
    _ -> pure unit

handleAction (PointOutput_EditorAction (MouseDown_PointOutput _event p)) = do
  state <- get
  mb_handle <- liftEffect do Ref.read state.ref_mb_handle
  case mb_handle of
    Nothing -> do
      liftEffect do state.ref_mb_dragOrigin := pure (Point_Handle p)
      setHandle $ pure (Point_Handle p)
    Just h -> do
      let dragOrigin = Expr.Drag.getDragOrigin h p
      liftEffect do state.ref_mb_dragOrigin := pure dragOrigin
      setHandle $ pure dragOrigin
handleAction (PointOutput_EditorAction (MouseEnter_PointOutput event p)) = do
  when (MouseEvent.buttons event == 1) do
    state <- get
    mb_dragOrigin <- liftEffect $ Ref.read state.ref_mb_dragOrigin
    case mb_dragOrigin of
      Nothing -> pure unit
      Just h -> do
        case state.root # Expr.Drag.drag h p of
          Nothing -> pure unit
          Just h' -> setHandle (pure h')
handleAction (PointOutput_EditorAction (BufferOutput_PointOutput (SubmitBuffer_BufferOutput bufferOption))) = do
  state <- get
  handle <- (liftEffect $ Ref.read state.ref_mb_handle) >>= fromMaybeM do liftEffect $ throw "impossible to submit Buffer when there is no Handle"
  case bufferOption of
    PasteSpan_BufferOption _ span -> do
      let root' /\ handle' = state.root # Expr.Edit.paste (Span_Fragment span) handle
      modifyEditorState _
        { root = root'
        , initial_mb_handle = pure handle'
        }

--------------------------------------------------------------------------------
-- undo and redo
--------------------------------------------------------------------------------

snapshot :: EditorM Unit
snapshot = do
  state <- get
  mb_handle <- liftEffect $ state.ref_mb_handle # Ref.read
  liftEffect $ state.ref_history :%= ({ root: state.root, mb_handle } : _)
  liftEffect $ state.ref_future := none

undo :: EditorM Unit
undo = do
  state <- get
  (liftEffect $ state.ref_history # Ref.read) >>= case _ of
    Nil -> pure unit
    s : history' -> do
      liftEffect $ state.ref_history := history'
      liftEffect $ state.ref_future :%= (s : _)
      loadSnapshot s

redo :: EditorM Unit
redo = do
  state <- get
  (liftEffect $ state.ref_future # Ref.read) >>= case _ of
    Nil -> pure unit
    s : future' -> do
      liftEffect $ state.ref_history :%= (s : _)
      liftEffect $ state.ref_future := future'
      loadSnapshot s

loadSnapshot :: Snapshot -> EditorM Unit
loadSnapshot s = do
  setHandle' do
    get >>= \state -> liftEffect $ state.ref_mb_dragOrigin := none
    state <- modify _ { root = s.root, initial_mb_handle = s.mb_handle }
    pure state.initial_mb_handle

--------------------------------------------------------------------------------
-- modifyEditorState
--------------------------------------------------------------------------------

modifyEditorState :: (EditorState -> EditorState) -> EditorM Unit
modifyEditorState f = do
  snapshot
  setHandle' do
    get >>= \state -> liftEffect $ state.ref_mb_dragOrigin := none
    state <- modify f
    pure state.initial_mb_handle

--------------------------------------------------------------------------------
-- setHandle
--------------------------------------------------------------------------------

-- | turns off old handle, then turns on new handle
setHandle :: Maybe Handle -> EditorM Unit
setHandle mb_handle = setHandle' $ pure mb_handle

-- | turns off old handle, then computes new handle, then turns on new handle.
-- | note that this DOES NOT reset ref_mb_dragOrigin.
setHandle' :: EditorM (Maybe Handle) -> EditorM Unit
setHandle' m_mb_handle = do
  state <- get
  mb_handle_old <- liftEffect $ Ref.read state.ref_mb_handle
  modifyHandle false mb_handle_old
  mb_handle_new <- m_mb_handle
  -- Console.log $ "[Editor] setHandle " <> show mb_handle_new
  modifyHandle true mb_handle_new
  liftEffect $ state.ref_mb_handle := mb_handle_new

modifyHandle :: Boolean -> Maybe Handle -> EditorM Unit
modifyHandle b mb_handle = do
  let
    modifyClass :: Point -> Set PointStatus -> EditorM Unit
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

render :: EditorState -> EditorHTML
render state =
  HHK.div [ classes [ "Editor" ] ]
    [ "root" /\
        HHK.div [ classes [ "root" ] ]
          [ "0" /\
              flip runReader state do
                renderExpr Nil state.root
          ]
    ]

type RenderM = Reader EditorState

renderExpr :: Path -> Expr L -> RenderM EditorHTML
renderExpr path (Expr e) = do
  htmls_kidsAndPoints <- Expr e # traverseStepsAndKids \i e_kid -> do
    html_p <- renderPoint $ Point { path, j: (i # getIndexesAroundStep)._L }
    html_e <- renderExpr (path `List.snoc` i) e_kid
    pure [ html_p, html_e ]
  html_lastPoint <- renderPoint $ Point { path, j: (Expr e # getExtremeIndexes)._R }
  pure
    $ HHK.div [ classes [ "Expr" ] ]
    $ mapWithIndex (\i -> (show i /\ _))
    $ Array.fold
        [ [ HH.div [ classes [ "Punctuation" ] ] [ HH.text "(" ] ]
        , [ HH.div [ classes [ "label" ] ] [ HH.text $ show e.l ] ]
        , Array.fold htmls_kidsAndPoints
        , [ html_lastPoint ]
        , [ HH.div [ classes [ "Punctuation" ] ] [ HH.text ")" ] ]
        ]

renderPoint :: Point -> RenderM EditorHTML
renderPoint point = do
  pure $ HH.slot (Proxy @"Point") point Point.component { point } PointOutput_EditorAction

