module Ui.App1.Editor where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (get)
import Data.Array as Array
import Data.Expr (Expr(..), Handle(..), Path, Point(..), SpanFocus(..), ZipperFocus(..), getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeIndexes, getFocusPoint, getIndexesAroundStep, traverseStepsAndKids)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Move as Expr.Move
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Unfoldable (none)
import Editor.Example.Editor2 (L)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.Event as HQE
import Type.Prelude (Proxy(..))
import Ui.App1.Common (EditorAction(..), EditorHTML, EditorInput, EditorM, EditorOutput, EditorQuery, EditorSlots, EditorState, PointOutput(..), PointQuery(..), PointStatus(..))
import Ui.App1.Point as Point
import Ui.Event (fromEventToKeyInfo, matchKeyInfo, matchMapKeyInfo) as Event
import Ui.Halogen (classes)
import Utility ((:=))
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
  , ref_mb_handle: unsafePerformEffect do Ref.new none
  , ref_mb_dragOrigin: unsafePerformEffect do Ref.new none
  }

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
handleAction (MouseUp_EditorAction _event) = do
  state <- get
  liftEffect do state.ref_mb_dragOrigin := none

handleAction (KeyDown_EditorAction event) = do
  state <- get
  let ki = Event.fromEventToKeyInfo event
  case unit of
    -- move
    _ | Just dir <- ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      liftEffect $ state.ref_mb_dragOrigin := none
      mb_handle <- liftEffect $ Ref.read state.ref_mb_handle
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
      mb_handle <- liftEffect $ Ref.read state.ref_mb_handle
      mb_dragOrigin <- liftEffect $ Ref.read state.ref_mb_dragOrigin
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
      mb_handle <- liftEffect $ Ref.read state.ref_mb_handle
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          liftEffect $ state.ref_mb_dragOrigin := none
          setHandle $ pure $ handle # Expr.Move.moveHandleFocus dir
    _ | ki # Event.matchKeyInfo (_ == "Escape") { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      liftEffect $ state.ref_mb_dragOrigin := none
      mb_handle <- liftEffect $ Ref.read state.ref_mb_handle
      case mb_handle of
        Nothing -> pure unit
        Just h -> setHandle $ Expr.Move.escape h
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

setHandle :: Maybe Handle -> EditorM Unit
setHandle mb_handle = do
  -- Console.log $ "[Editor] setHandle " <> show mb_handle
  state <- get
  mb_handle_old <- liftEffect $ Ref.read state.ref_mb_handle
  modifyHandle false mb_handle_old
  modifyHandle true mb_handle
  liftEffect $ state.ref_mb_handle := mb_handle

modifyHandle :: Boolean -> Maybe Handle -> EditorM Unit
modifyHandle b mb_handle = do
  let
    modifyClass :: Point -> Set PointStatus -> EditorM Unit
    modifyClass p ss' =
      if b then
        H.tell (Proxy @"Point") p $ ModifyMaybeStatuses_PointQuery (_ `Set.union` ss')
      else
        H.tell (Proxy @"Point") p $ ModifyMaybeStatuses_PointQuery (_ `Set.difference` ss')
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
  HH.div [ classes [ "Editor" ] ]
    [ HH.div [ classes [ "root" ] ]
        [ flip runReader state $
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
    $ HH.div [ classes [ "Expr" ] ]
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

