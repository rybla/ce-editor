module Ui.Engine where

import Data.Expr
import Ui.Types
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (get, gets, modify_, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Either (Either(..))
import Data.Expr.Drag (drag, getDragOrigin)
import Data.Expr.Move (move)
import Data.Expr.Move as Expr.Move
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Type.Prelude (Proxy(..))
import Ui.Common (KeyInfo(..), classes, code, column, fromKeyboardEventToKeyInfo, list, matchKeyInfo, matchMapKeyInfo, style, text)
import Ui.Common as Ui
import Ui.Console as Console
import Utility (forget, isAlpha, sortEquivalenceClasses, todo)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEvent.EventType
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEventType

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
            traceEngineM [ "Error" ] err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleEngineAction action # runExceptT >>= case _ of
          Left err -> do
            put state
            traceEngineM [ "Error" ] err
          Right it -> pure it
    }

  render = const $ HH.div [ style do tell [ "display: hidden" ] ] []

initialEngineState :: EngineInput -> EngineState
initialEngineState input =
  { editor: input.editor
  , expr: input.expr
  , handle: input.handle
  , history: mempty
  , future: mempty
  , drag_origin_handle: Nothing
  , clipboard: Nothing
  , bufferEnabled: false
  }

handleEngineQuery :: forall a. EngineQuery a -> EngineM' a
handleEngineQuery (ExprInteraction_EngineQuery path ei a) = case ei of
  Click_ViewExprAction _event -> do
    lift $ traceEngineM [ "Expr", "Click" ] $ text $ "got Click from Expr at " <> show path
    case path # List.unsnoc of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        let last_j = getIndexesAroundStep last
        let h = SpanH_Handle (SpanH { path: init, j_L: last_j._L, j_R: last_j._R }) Left_SpanFocus
        change_handle h
    pure a
  StartDrag_ViewExprAction _event -> do
    case path # List.unsnoc of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        -- the point right before the expr
        let p = Point { path: init, j: (getIndexesAroundStep last)._L }
        lift $ traceEngineM [ "Drag" ] $ text $ "got StartDrag from Expr at " <> show p
        h <- gets _.handle
        let h' = getDragOrigin h p
        modify_ _ { drag_origin_handle = Just h' }
        change_handle h'
    pure a
  MidDrag_ViewExprAction _event -> do
    case path # List.unsnoc of
      Nothing -> pure unit -- this really shouldnt happen though...
      Just { init, last } -> do
        -- the point right before the expr
        let p = Point { path: init, j: (getIndexesAroundStep last)._L }
        lift $ traceEngineM [ "Drag" ] $ text $ "got MidDrag from Expr at " <> show p
        updateDragToPoint p
    pure a
handleEngineQuery (ViewPointInteraction_EngineQuery p pi a) = case pi of
  StartDrag_ViewPointInteraction _event -> do
    lift $ traceEngineM [ "Drag" ] $ text $ "got StartDrag from Point at " <> show p
    h <- gets _.handle
    let h' = getDragOrigin h p
    modify_ _ { drag_origin_handle = Just h' }
    change_handle h'
    pure a
  MidDrag_ViewPointInteraction _event -> do
    lift $ traceEngineM [ "Drag" ] $ text $ "got MidDrag from Point at " <> show p
    updateDragToPoint p
    pure a
handleEngineQuery (EndDrag_EngineQuery a) = do
  lift $ traceEngineM [ "Drag" ] $ text $ "got EndDrag"
  modify_ _ { drag_origin_handle = Nothing }
  pure a

handleEngineAction :: EngineAction -> EngineM' Unit
handleEngineAction Initialize_EngineAction = do
  lift $ traceEngineM [ "Initialize" ] $ text "initialize"
  st <- get
  do
    doc <- liftEffect $ Window.document =<< HTML.window
    lift $ H.subscribe' \_subId ->
      HQE.eventListener
        KeyboardEvent.EventType.keydown
        (HTMLDocument.toEventTarget doc)
        (KeyboardEvent.fromEvent >>> map (fromKeyboardEventToKeyInfo >>> Keyboard_EngineAction))
  do
    change_handle st.handle
handleEngineAction (Receive_EngineAction input) = do
  lift $ traceEngineM [ "Receive" ] $ text "receive"
  do
    st <- get
    put $ (initialEngineState input)
      -- preserve some stuff from old state
      { clipboard = st.clipboard
      , history = st.history
      , future = st.future
      }
handleEngineAction (Keyboard_EngineAction (KeyInfo ki)) = do
  lift $ traceEngineM [ "Keyboard" ] $ text $ "key: " <> show ki
  { handle, clipboard, expr, editor } <- get
  case unit of
    -- move
    _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure false } -> do
      case handle # move expr dir of
        Nothing -> pure unit
        Just p' -> do
          lift $ traceEngineM [ "Move" ] $ code (show p')
          change_handle $ Point_Handle $ p'
    -- move + select
    _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: pure false, shift: pure true } -> do
      case handle # move expr dir of
        Nothing -> pure unit
        Just p' -> case drag handle p' expr of
          Nothing -> pure unit
          Just handle' -> do
            lift $ traceEngineM [ "Move", "Select" ] $ code (show handle')
            change_handle handle'
    -- undo/redo
    _ | KeyInfo ki # matchKeyInfo (_ == "z") { cmd: pure true, shift: pure false } -> undo
    _ | KeyInfo ki # matchKeyInfo (_ == "z") { cmd: pure true, shift: pure true } -> redo
    -- escape
    _ | KeyInfo ki # matchKeyInfo (_ == "Escape") {} -> do
      case handle of
        Point_Handle _ -> change_handle $ Point_Handle $ Point { path: mempty, j: Index 0 }
        SpanH_Handle (SpanH h) Left_SpanFocus -> change_handle $ Point_Handle $ Point { path: h.path, j: h.j_R }
        SpanH_Handle (SpanH h) Right_SpanFocus -> change_handle $ Point_Handle $ Point { path: h.path, j: h.j_L }
        ZipperH_Handle (ZipperH h) OuterLeft_ZipperFocus -> change_handle $ SpanH_Handle (SpanH { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j_L: h.j_IL, j_R: h.j_IR }) Right_SpanFocus
        ZipperH_Handle (ZipperH h) OuterRight_ZipperFocus -> change_handle $ SpanH_Handle (SpanH { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j_L: h.j_IL, j_R: h.j_IR }) Left_SpanFocus
        ZipperH_Handle (ZipperH h) InnerLeft_ZipperFocus -> change_handle $ SpanH_Handle (SpanH { path: h.path_O, j_L: h.j_OL, j_R: h.j_OR }) Right_SpanFocus
        ZipperH_Handle (ZipperH h) InnerRight_ZipperFocus -> change_handle $ SpanH_Handle (SpanH { path: h.path_O, j_L: h.j_OL, j_R: h.j_OR }) Left_SpanFocus
      pure unit
    -- copy Fragment
    _ | KeyInfo ki # matchKeyInfo (_ == "c") { cmd: pure true } -> do
      let
        frag = case handle of
          Point_Handle _ -> Span_Fragment (Span [])
          SpanH_Handle h _ -> Span_Fragment (atSpan h expr).at
          ZipperH_Handle h _ -> Zipper_Fragment (atZipper h expr).at
      lift $ traceEngineM [ "Clipboard", "Copy" ] $ Ui.list
        [ Ui.code $ "frag: " <> show frag ]
      modify_ _ { clipboard = pure $ frag }
    -- cut Fragment
    _ | KeyInfo ki # matchKeyInfo (_ == "x") { cmd: pure true } -> do
      let
        frag /\ expr' = case handle of
          Point_Handle _ -> Span_Fragment (Span []) /\ expr
          SpanH_Handle h _ -> Span_Fragment at_h.at /\ unSpanContext at_h.outside (Span [])
            where
            at_h = expr # atSpan h
          ZipperH_Handle h _ -> Zipper_Fragment at_h.at /\ unSpanContext at_h.outside at_h.inside
            where
            at_h = expr # atZipper h
      -- lift $ traceEngineM [ "Clipboard", "Cut" ] $ Ui.list
      --   [ Ui.code $ "handle#getOuterLeftPoint_Handle: " <> show (handle # getOuterLeftPoint_Handle) ]
      lift $ traceEngineM [ "Clipboard", "Cut" ] $ Ui.list
        [ Ui.code $ "frag: " <> show frag ]
      modify_ _ { clipboard = pure $ frag }
      unset_handle handle
      set_expr expr'
      -- TODO: this was an attempt to see if the problem was a desync of effects via raising->querying
      -- void $ liftAff $ Aff.delay $ Aff.Milliseconds 100.0 
      set_handle $ Point_Handle $ handle # getOuterLeftPoint_Handle
    _ | KeyInfo ki # matchKeyInfo (_ == "Backspace") {} -> do
      let
        expr' = case handle of
          Point_Handle _ -> expr
          SpanH_Handle h _ -> unSpanContext at_h.outside (Span [])
            where
            at_h = expr # atSpan h
          ZipperH_Handle h _ -> unSpanContext at_h.outside at_h.inside
            where
            at_h = expr # atZipper h
      set_expr expr'
      change_handle $ Point_Handle $ handle # getOuterLeftPoint_Handle
    -- paste Fragment
    _ | Just frag <- clipboard, KeyInfo ki # matchKeyInfo (_ == "v") { cmd: pure true } -> do
      lift $ traceEngineM [ "Clipboard", "Paste" ] $ Ui.list
        [ Ui.code $ "frag: " <> show frag ]
      insert_fragment frag
      do
        st <- get
        lift $ traceEngineM [ "Clipboard", "Paste" ] $ Ui.code $ "handle: " <> show st.handle
    -- toggle Buffer
    _ | KeyInfo ki # matchKeyInfo (_ == "Enter") { cmd: pure false } -> do
      lift $ traceEngineM [ "Buffer" ] $ text "toggle"
      doSingleViewExprQueries =<< do modify_bufferEnabled not
    -- insert example Fragment 
    _ | Just frag <- editor.example_fragment ki.key, KeyInfo ki # matchKeyInfo isAlpha {} -> do
      lift $ traceEngineM [ "Insert" ] $ Ui.list
        [ Ui.code $ "frag: " <> show frag ]
      insert_fragment frag
    _ -> pure unit
  pure unit

set_expr :: Expr -> EngineM' Unit
set_expr expr = do
  snapshot
  lift $ H.raise $ SetExpr_EngineOutput expr

snapshot :: EngineM' Unit
snapshot = do
  { editor, handle, expr, history } <- get
  let s = { handle, expr }
  case history of
    Nil -> modify_ \st -> st { history = s : Nil }
    s' : _
      | s == s' -> pure unit
      | otherwise -> modify_ \st -> st { history = s : history # List.take editor.max_history_length, future = mempty }
  { history: history' } <- get
  -- lift $ traceEngineM "Snapshot" $ Ui.span [ Ui.text $ "history' length = " <> show (history' # List.length) ]
  lift $ traceEngineM [ "Snapshot" ] $ Ui.column
    [ Ui.text "history:"
    , Ui.list
        [ Ui.code $ "length: " <> show (history' # List.length)
        , Ui.column $ Array.fold
            [ [ Ui.code "head:" ]
            , case history' # List.head of
                Nothing -> [ Ui.code "Nothing" ]
                Just head ->
                  [ Ui.list
                      [ Ui.code $ "expr: " <> show head.expr
                      , Ui.code $ "handle: " <> show head.handle
                      ]
                  ]
            ]
        ]
    ]

undo :: EngineM' Unit
undo = do
  { handle, expr, history, future } <- get
  case history of
    Nil -> pure unit
    s : history' -> do
      modify_ _ { history = history', future = { handle, expr } : future }
      lift $ H.raise $ SetExpr_EngineOutput s.expr
      change_handle s.handle

redo :: EngineM' Unit
redo = do
  { handle, expr, history, future } <- get
  case future of
    Nil -> pure unit
    s : future' -> do
      modify_ _ { future = future', history = { handle, expr } : history }
      lift $ H.raise $ SetExpr_EngineOutput s.expr
      change_handle s.handle

insert_fragment :: Fragment -> EngineM' Unit
insert_fragment frag = do
  { handle, expr } <- get
  handle' /\ expr' <- case handle of
    Point_Handle (Point p) -> case frag of
      Span_Fragment f -> pure $ Tuple
        (Point_Handle (Point { path: p.path, j: p.j + (f # offset_Span) }))
        (unSpanContext at_h.outside f)
      -- pasting a Zipper around a Span
      Zipper_Fragment (Zipper z) -> Tuple
        <$>
          ( pure $ Point_Handle
              ( Point
                  { path: p.path <> ((p.j # getStepsAroundIndex)._L : Nil) <> (z.inside # getPath_SpanContext)
                  , j: Zipper z # offset_inner_Zipper
                  }
              )
          )
        <*> pure (unSpanContext at_h.outside $ unZipper (Zipper z) (Span []))
      where
      at_h = expr # atPoint (Point p)
    SpanH_Handle (SpanH h) focus -> case frag of
      -- pasting a Span in place of a Span
      Span_Fragment f -> pure $ Tuple
        ( case focus of
            Left_SpanFocus -> Point_Handle (Point { path: h.path, j: h.j_L })
            Right_SpanFocus -> Point_Handle (Point { path: h.path, j: h.j_L + (f # offset_Span) })
        )
        (unSpanContext at_h.outside f)
      -- pasting a Zipper around a Span
      Zipper_Fragment (Zipper z) -> Tuple
        <$>
          ( pure $ Point_Handle
              ( Point
                  { path: h.path <> (h.j_L # getStepsAroundIndex)._R : Nil <> (z.inside # getPath_SpanContext)
                  , j: Zipper z # offset_inner_Zipper
                  }
              )
          )
        <*> pure (unSpanContext at_h.outside $ unZipper (Zipper z) at_h.at)
      where
      at_h = expr # atSpan (SpanH h)
    ZipperH_Handle h _focus -> case frag of
      Span_Fragment f -> pure $ Tuple
        (todo "new handle")
        (unSpanContext at_h.outside f)
      Zipper_Fragment f -> pure $ Tuple
        (todo "new handle")
        (unSpanContext at_h.outside $ unZipper f at_h.inside)
      where
      at_h = expr # atZipper h
  set_expr expr'
  change_handle handle'

--------------------------------------------------------------------------------

updateDragToPoint :: Point -> EngineM' Unit
updateDragToPoint p = do
  e <- gets _.expr
  gets _.drag_origin_handle >>= case _ of
    Nothing -> pure unit
    Just drag_origin_handle -> case drag drag_origin_handle p e of
      Nothing -> do
        lift $ traceEngineM [ "Drag" ] $
          column
            [ text $ "updateDragToPoint (failure)"
            , list
                [ text $ "drag_origin_handle = " <> show drag_origin_handle
                , text $ "                 p = " <> show p
                ]
            ]
        pure unit
      Just h' -> do
        lift $ traceEngineM [ "Drag" ] $
          column
            [ text $ "updateDragToPoint (success)"
            , list
                [ text $ "drag_origin_handle = " <> show drag_origin_handle
                , text $ "                 p = " <> show p
                , text $ "            drag ==> " <> show h'
                ]
            ]
        change_handle h'

-- | Unsets the handle. This is used in unsetting the old handle before updating
-- | the expr, and then setting the new handle.
unset_handle :: Handle -> EngineM' Unit
unset_handle h = do
  unset_bufferEnabled
  doSingleViewExprQueries do
    toggleViewPointStyles $ toggleHandleViewPointStyles false h

-- | Sets the handle. Assumes that the old handle has already been unset. This
-- | is used in unsetting the old handle before updating the expr, and then
-- | setting the new handle.
set_handle :: Handle -> EngineM' Unit
set_handle h = do
  doSingleViewExprQueries do
    toggleViewPointStyles $ toggleHandleViewPointStyles true h

-- | Unsets the old handle and sets the new handle at the same time.
change_handle :: Handle -> EngineM' Unit
change_handle h = do
  unset_bufferEnabled
  doSingleViewExprQueries =<< do
    vps1 <- toggleHandleViewPointStyles false <$> gets _.handle
    modify_ _ { handle = h }
    lift $ traceEngineM [ "Drag" ] $ Ui.span [ text "new handle: ", code (show h) ]
    let vps2 = toggleHandleViewPointStyles true h
    pure $ toggleViewPointStyles $ Map.unionWith forget vps1 vps2

--------------------------------------------------------------------------------

doSingleViewExprQueries :: Array SingleViewExprQuery -> EngineM' Unit
doSingleViewExprQueries qs = case qs # NEArray.fromArray of
  Nothing -> pure unit
  Just qs' -> lift $ H.raise $ ViewExprQuery_EngineOutput (ViewExprQuery qs')

toggleHandleViewPointStyles :: Boolean -> Handle -> Map Point ViewPointStyle
toggleHandleViewPointStyles active (Point_Handle p) = Map.fromFoldable [ p /\ toggleViewPointStyle active Point_ViewPointStyle ]
toggleHandleViewPointStyles active (SpanH_Handle h _focus) = Map.fromFoldable [ hp._L /\ toggleViewPointStyle active Span_Left_ViewPointStyle, hp._R /\ toggleViewPointStyle active Span_Right_ViewPointStyle ]
  where
  hp = getEndPoints_SpanH h
toggleHandleViewPointStyles active (ZipperH_Handle (ZipperH h) _focus) = case unit of
  _ | hp._IL == hp._IR -> Map.fromFoldable [ hp._OL /\ toggleViewPointStyle active Zipper_OuterLeft_ViewPointStyle, hp._IL /\ toggleViewPointStyle active Zipper_Inline_InnerLeft_And_InnerRight_ViewPointStyle, hp._OR /\ toggleViewPointStyle active Zipper_OuterRight_ViewPointStyle ]
  _ | hp._OL == hp._IL -> Map.fromFoldable [ hp._OL /\ toggleViewPointStyle active Zipper_Inline_OuterLeft_And_InnerLeft_ViewPointStyle, hp._IR /\ toggleViewPointStyle active Zipper_InnerRight_ViewPointStyle, hp._OR /\ toggleViewPointStyle active Zipper_OuterRight_ViewPointStyle ]
  _ | hp._IR == hp._OR -> Map.fromFoldable [ hp._OL /\ toggleViewPointStyle active Zipper_OuterLeft_ViewPointStyle, hp._IL /\ toggleViewPointStyle active Zipper_InnerLeft_ViewPointStyle, hp._IR /\ toggleViewPointStyle active Zipper_Inline_InnerRight_And_OuterRight_ViewPointStyle ]
  _ | hp._OL == hp._IL -> Map.fromFoldable [ hp._OL /\ toggleViewPointStyle active Zipper_OuterLeft_And_InnerLeft_ViewPointStyle, hp._IR /\ toggleViewPointStyle active Zipper_InnerRight_ViewPointStyle, hp._OR /\ toggleViewPointStyle active Zipper_OuterRight_ViewPointStyle ]
  _ | hp._IR == hp._OR -> Map.fromFoldable [ hp._OL /\ toggleViewPointStyle active Zipper_OuterLeft_ViewPointStyle, hp._IL /\ toggleViewPointStyle active Zipper_InnerLeft_ViewPointStyle, hp._IR /\ toggleViewPointStyle active Zipper_InnerRight_And_OuterRight_ViewPointStyle ]
  _ | hp._IL == hp._IR -> Map.fromFoldable [ hp._OL /\ toggleViewPointStyle active Zipper_OuterLeft_ViewPointStyle, hp._IL /\ toggleViewPointStyle active Zipper_InnerLeft_And_InnerRight_ViewPointStyle, hp._OR /\ toggleViewPointStyle active Zipper_OuterRight_ViewPointStyle ]
  _ -> Map.fromFoldable [ hp._OL /\ toggleViewPointStyle active Zipper_OuterLeft_ViewPointStyle, hp._IL /\ toggleViewPointStyle active Zipper_InnerLeft_ViewPointStyle, hp._IR /\ toggleViewPointStyle active Zipper_InnerRight_ViewPointStyle, hp._OR /\ toggleViewPointStyle active Zipper_OuterRight_ViewPointStyle ]
  where
  hp = getEndPoints_ZipperH (ZipperH h)

toggleViewPointStyles :: Map Point ViewPointStyle -> Array SingleViewExprQuery
toggleViewPointStyles xs = xs
  # mapWithIndex (\p s -> mkViewPointQuery_SingleViewExprQuery p $ SetViewPointStyle_ViewPointQuery s unit)
  # Array.fromFoldable

toggleViewPointStyle :: Boolean -> ViewPointStyle -> ViewPointStyle
toggleViewPointStyle false = const Plain_ViewPointStyle
toggleViewPointStyle true = identity

unset_bufferEnabled :: EngineM' Unit
unset_bufferEnabled = modify_ _ { bufferEnabled = false }

modify_bufferEnabled :: (Boolean -> Boolean) -> EngineM' (Array SingleViewExprQuery)
modify_bufferEnabled f = do
  { handle, bufferEnabled } <- get
  let bufferEnabled' = f bufferEnabled
  if bufferEnabled /= bufferEnabled' then do
    modify_ _ { bufferEnabled = bufferEnabled' }
    pure $ Array.singleton
      $ mkViewPointQuery_SingleViewExprQuery (getOuterLeftPoint_Handle handle)
      $ SetBufferEnabled_ViewPointQuery bufferEnabled' unit
  else
    pure []

--------------------------------------------------------------------------------

traceEngineM :: Array String -> PlainHTML -> EngineM Unit
traceEngineM labels content = H.raise $ Output_EngineOutput $ TellConsole \a -> Console.AddMessage { labels: [ "Engine" ] <> labels, content } a
