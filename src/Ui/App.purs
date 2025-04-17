module Ui.App where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Expr (Diff(..), Expr(..), Handle(..), Index(..), NePath, Path, Point(..), Span(..), SpanFocus(..), SpanH(..), Step(..), Tooth(..), Zipper(..), ZipperFocus(..), atIndexSpan_Expr, atInjectDiff, atPoint, atSpan, atSubExpr, defaultHandle, getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeSteps, getFocusPoint, getIndexesAroundStep, getKid_Expr, mkExpr, offset_Span, rangeKidSteps, rangeSteps, toNePath)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Move as Expr.Move
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Trident as Trident
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor.Example.Editor1 (Dat(..), L(..), mkL)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Ui.Common (EventListenerInfo, KeyInfo(..), addClass, addEventListenerWithOptions, appendChild, body, createElement, createElement_orphan, doc, fromEventToKeyInfo, matchKeyInfo, matchMapKeyInfo, removeAllChildren, removeChild, removeClass, removeEventListener, replaceChild, setText_Element, shiftKey)
import Utility (fromMaybeM, isAlpha, todo, (:%=), (:=))
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..), preventDefault)

--------------------------------------------------------------------------------
-- config
--------------------------------------------------------------------------------

initialExpr_1 = "A" % [ "B" % [], "C" % [] ]

config =
  { initialExpr: initialExpr_1
  -- , initialExpr: example_expr {} 2 2
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
-- State
--------------------------------------------------------------------------------

type State =
  { mb_uiExprRoot :: Ref (Maybe UiExpr)
  , mb_handle :: Ref (Maybe Handle)
  , mb_dragOrigin :: Ref (Maybe Handle)
  }

newState :: Effect State
newState = do
  mb_uiExprRoot <- Ref.new Nothing
  mb_handle <- Ref.new Nothing
  mb_dragOrigin <- Ref.new Nothing
  pure { mb_uiExprRoot, mb_handle, mb_dragOrigin }

--------------------------------------------------------------------------------
-- UiEditor
--------------------------------------------------------------------------------

type UiEditor =
  { elem :: Element
  , eventListenerInfos :: Array EventListenerInfo
  }

--------------------------------------------------------------------------------
-- PureExpr
--------------------------------------------------------------------------------

type PureExpr = Expr PureLabel
type PureLabel = L {}

mkPureExpr ∷ String → Array PureExpr → PureExpr
mkPureExpr s kids = mkExpr (L { dat: String s, meta: {} }) kids

infix 4 mkPureExpr as %

--------------------------------------------------------------------------------
-- UiExpr
--------------------------------------------------------------------------------

type UiExpr = Expr UiLabel
type UiLabel = L UiExprMeta

type UiExprMeta =
  { elem :: Element
  , eventListenerInfos :: Array EventListenerInfo
  , path :: Ref Path
  -- indexed by each Point Index
  , uiPoints :: Array UiPoint
  }

type UiPoint =
  { elem :: Element
  , eventListenerInfos :: Array EventListenerInfo
  , point :: Ref Point
  }

getElem_UiExpr :: UiExpr -> Element
getElem_UiExpr e = ((e # unwrap).l # unwrap).meta.elem

getUiPoints_UiExpr :: UiExpr -> Array UiPoint
getUiPoints_UiExpr e = ((e # unwrap).l # unwrap).meta.uiPoints

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

  expr' <- elem # createUiExpr state Nil expr
  state.mb_uiExprRoot := pure expr'

  eventListenerInfos <- sequence
    [ state # eventListenerInfo_stopDrag_Editor
    , state # eventListenerInfo_keydown_Editor
    ]

  pure { elem, eventListenerInfos }

eventListenerInfo_stopDrag_Editor :: State -> Effect EventListenerInfo
eventListenerInfo_stopDrag_Editor state = doc # Document.toEventTarget # addEventListenerWithOptions (EventType "mouseup") { capture: true, passive: true } \_event -> do
  state.mb_dragOrigin := none

eventListenerInfo_keydown_Editor :: State -> Effect EventListenerInfo
eventListenerInfo_keydown_Editor state = doc # Document.toEventTarget # addEventListenerWithOptions (EventType "keydown") { capture: true } \event -> do
  let KeyInfo ki = event # fromEventToKeyInfo
  -- Console.log $ "keydown: " <> ki.key
  mb_dragOrigin <- state.mb_dragOrigin # Ref.read
  mb_handle <- state.mb_handle # Ref.read
  case unit of
    -- move
    _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just false, alt: Just false } -> do
      event # preventDefault
      state.mb_dragOrigin := none
      case mb_handle of
        Nothing -> do
          state # setHandle (Just defaultHandle)
        Just handle -> do
          uiExprRoot <- state # getUiExpr_root
          case Expr.Move.movePoint uiExprRoot dir (handle # getFocusPoint) of
            Nothing -> state # setHandle (Just (Point_Handle (handle # getFocusPoint)))
            Just point -> state # setHandle (Just (Point_Handle point))
    -- drag move
    _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just true, alt: Just false } -> do
      Console.log "drag move"
      event # preventDefault
      case mb_handle of
        Nothing -> do
          -- initialize dragOrigin
          case mb_dragOrigin of
            Nothing -> do
              state.mb_dragOrigin := pure defaultHandle
            Just _ -> do
              pure unit
          state # setHandle (Just defaultHandle)
        Just handle -> do
          uiExprRoot <- state # getUiExpr_root
          -- initialize dragOrigin
          dragOrigin <- case mb_dragOrigin of
            Nothing -> do
              Console.log "initialize dragOrigin"
              state.mb_dragOrigin := pure handle
              pure handle
            Just dragOrigin -> do
              pure dragOrigin
          case Expr.Move.movePointUntil uiExprRoot dir (handle # getFocusPoint) \p -> uiExprRoot # Expr.Drag.drag dragOrigin p of
            Nothing -> pure unit
            Just handle' -> do
              state # setHandle (Just handle')
    -- move handle focus
    _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just false, alt: Just true } -> do
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          state # setHandle (Just (handle # Expr.Move.moveHandleFocus dir))
    -- insert
    _ | KeyInfo ki # matchKeyInfo isAlpha { cmd: pure false, alt: pure false } -> do
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          expr <- state # getUiExpr_root
          case handle of
            Point_Handle (Point p) -> do
              case p.path # toNePath of
                Nothing -> pure unit
                Just nepath -> do
                  let
                    t = Tooth { l: mkL (String ki.key) {}, kids_L: [], kids_R: [] }
                    diff = expr # atInjectDiff nepath \_expr' ->
                      InsertTooth_Diff t Id_Diff
                  Console.log $ "diff = " <> show diff
                  state # setHandle none
                  state # updateUiExprViaDiff_root diff
            _ -> pure unit -- TODO: other kinds of insert
          pure unit
    -- test 1
    _ | KeyInfo ki # matchKeyInfo (_ == "1") { cmd: pure true } -> do
      event # preventDefault
      state.mb_dragOrigin := none
      state # setHandle Nothing
      state # updateUiExprViaDiff_root
        ( Inject_Diff {- Root -}
            [ DeleteTooth_Diff {- A -}  (Step 0)
                (Inject_Diff {- C -}  [])
            ]
        )
    -- test 2
    _ | KeyInfo ki # matchKeyInfo (_ == "2") { cmd: pure true } -> do
      event # preventDefault
      state.mb_dragOrigin := none
      state # setHandle Nothing
      state # updateUiExprViaDiff_root
        ( Inject_Diff {- Root -}
            [ Replace_Diff (mkPureExpr "D" [])
            ]
        )
    -- test 3
    _ | KeyInfo ki # matchKeyInfo (_ == "3") { cmd: pure true } -> do
      event # preventDefault
      state.mb_dragOrigin := none
      state # setHandle Nothing
      state # updateUiExprViaDiff_root
        ( Inject_Diff {- Root -}
            [ InsertTooth_Diff
                ( Tooth
                    { l: L { dat: String "D", meta: {} }
                    , kids_L: [ mkPureExpr "E" [] ]
                    , kids_R: [ mkPureExpr "F" [] ]
                    }
                ) $
                Inject_Diff {- A -}
                  [ Inject_Diff {- B -}  []
                  , Inject_Diff {- C -}  []
                  ]
            ]
        )
    _ -> pure unit

eventListenerInfo_keyup_Editor :: State -> Effect EventListenerInfo
eventListenerInfo_keyup_Editor state = doc # Document.toEventTarget # addEventListenerWithOptions (EventType "keyup") { capture: true } \event -> do
  let KeyInfo ki = event # fromEventToKeyInfo
  mb_handle <- state.mb_handle # Ref.read
  mb_dragOrigin <- state.mb_dragOrigin # Ref.read
  case unit of
    _ -> pure unit
  pure unit

--------------------------------------------------------------------------------
-- createUiExpr
--------------------------------------------------------------------------------

assembleUiExpr :: Ref Path -> Element -> PureLabel -> Array UiExpr -> State -> Effect UiExpr
assembleUiExpr path_ref elem_expr label kids_ state = do
  path <- path_ref # Ref.read

  -- attributes
  elem_expr # addClass "Expr"
  case config.displayStyle of
    Inline_DisplayStyle -> elem_expr # addClass "Inline"
    Nested_DisplayStyle -> elem_expr # addClass "Nested"
  case label of
    L { dat: Root } -> elem_expr # addClass "Root"
    _ -> pure unit

  eventListenerInfos <- sequence
    []

  -- open
  when (config.displayStyle == Inline_DisplayStyle) do
    elem_open <- elem_expr # createElement "div"
    elem_open # addClass "Punctuation"
    elem_open # Element.toNode # Node.setTextContent case label of
      L { dat: Root } -> ""
      _ -> "("

  -- label
  do
    elem_label <- elem_expr # createElement "div"
    elem_label # addClass "Label"
    elem_label # Element.toNode # Node.setTextContent (show label)

  -- kids
  kids /\ uiPoints_init <- map Array.unzip $ kids_ # traverseWithIndex \i_ kid -> do
    let i = Step i_
    let j = (i # getIndexesAroundStep)._L
    uiPoint <- elem_expr # createUiPoint state (Point { path, j })
    pure $ kid /\ uiPoint
  uiPoint_last <- elem_expr # createUiPoint state
    ( Point
        { path
        , j: kids_ # Span # offset_Span
        }
    )
  let uiPoints = uiPoints_init `Array.snoc` uiPoint_last

  -- close
  when (config.displayStyle == Inline_DisplayStyle) do
    elem_close <- elem_expr # createElement "div"
    elem_close # addClass "Punctuation"
    elem_close # Element.toNode # Node.setTextContent case label of
      L { dat: Root } -> ""
      _ -> ")"

  pure $ Expr
    { l: label # Newtype.over L _
        { meta =
            { elem: elem_expr
            , eventListenerInfos
            , path: path_ref
            , uiPoints
            }
        }
    , kids
    }

createUiExpr :: State -> Path -> PureExpr -> Element -> Effect UiExpr
createUiExpr state path (Expr expr) parent = do
  Console.log $ "createUiExpr: " <> show (Expr expr)

  parent # createUiExpr' state path expr.l (Expr expr # rangeKidSteps) \i elem_expr -> do
    kid <- Expr expr # getKid_Expr i # fromMaybeM do throw $ "kid index out of bounds"
    elem_expr # createUiExpr state (path `List.snoc` i) kid

createUiExpr' :: State -> Path -> PureLabel -> Array Step -> (Step -> Element -> Effect UiExpr) -> Element -> Effect UiExpr
createUiExpr' state path0 label steps renderKid parent = do
  path_ref <- Ref.new path0

  elem_expr <- parent # createElement "div"
  kids <- steps # traverse \i -> elem_expr # renderKid i
  state # assembleUiExpr path_ref elem_expr label kids

--------------------------------------------------------------------------------
-- createUiPoint
--------------------------------------------------------------------------------

createUiPoint :: State -> Point -> Element -> Effect UiPoint
createUiPoint state (Point point0) parent = do
  Console.log $ "createUiPoint " <> show (Point point0)

  pointRef <- Ref.new (Point point0)

  elem <- parent # createElement "div"
  elem # addClass "Point"

  eventListenerInfos <- sequence
    [ elem # Element.toEventTarget # addEventListenerWithOptions (EventType "mousedown") { capture: true, passive: true } \event -> do
        Point point <- pointRef # Ref.read
        state.mb_handle # Ref.read >>= case _ of
          Just h | event # shiftKey -> do
            expr <- state # getUiExpr_root
            state.mb_dragOrigin := pure h
            case Expr.Drag.drag h (Point point) expr of
              Nothing -> pure unit
              Just h' -> state # setHandle (pure h')
          Just h -> do
            uiExprRoot <- state # getUiExpr_root
            let dragOrigin = Expr.Drag.getDragOrigin h (Point point)
            state.mb_dragOrigin := pure dragOrigin
            case Expr.Drag.drag dragOrigin (Point point) uiExprRoot of
              Nothing -> pure unit
              Just h' -> state # setHandle (pure h')
          _ -> do
            let h' = Point_Handle (Point point)
            state.mb_dragOrigin := pure h'
            state # setHandle (pure h')
    , elem # Element.toEventTarget # addEventListenerWithOptions (EventType "mouseenter") { capture: true, passive: true } \_event -> do
        Point point <- pointRef # Ref.read
        -- Console.log $ "Point: mouseenter" <> show (Point point)
        mb_dragOrigin <- state.mb_dragOrigin # Ref.read
        case mb_dragOrigin of
          Nothing -> pure unit
          Just h -> do
            uiExprRoot <- state # getUiExpr_root
            case Expr.Drag.drag h (Point point) uiExprRoot of
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
    , point: pointRef
    }

--------------------------------------------------------------------------------
-- Diff
--------------------------------------------------------------------------------

updateUiExprViaDiff_root :: Diff PureLabel -> State -> Effect Unit
updateUiExprViaDiff_root diff0 state = do
  uiExprRoot <- state # getUiExpr_root
  uiExprRoot' <- state # updateUiExprViaDiff false Nil Nothing uiExprRoot diff0
  state.mb_uiExprRoot := pure uiExprRoot'

-- | Updates the UiExpr and all of its descendants, which updates each UiExpr's
-- | state and Ui elements.
updateUiExpr :: Path -> UiExpr -> State -> Effect UiExpr
updateUiExpr path (Expr e) state = do
  (e.l # unwrap).meta.path := path
  (e.l # unwrap).meta.uiPoints # traverse_ \uiPoint -> uiPoint.point :%= Newtype.modify _ { path = path }
  kids' <- e.kids # traverseWithIndex \i_ kid -> do
    let i = Step i_
    state # updateUiExpr (path `List.snoc` i) kid
  pure $ Expr e { kids = kids' }

-- | Updates the UiExpr and all of its descendants via a Diff, which applies the
-- | Diff's edit to the Expr and updates Ui elements accordingly.
updateUiExprViaDiff :: Boolean -> Path -> Maybe Element -> UiExpr -> Diff PureLabel -> State -> Effect UiExpr

updateUiExprViaDiff isMoved path _ expr Id_Diff state = do
  -- only need to update if has moved
  if not isMoved then pure expr
  else do
    state # updateUiExpr path expr

updateUiExprViaDiff _ path _ (Expr e) (Inject_Diff ds) state = do
  (e.l # unwrap).meta.path := path
  (e.l # unwrap).meta.uiPoints # traverse_ \uiPoint -> uiPoint.point :%= Newtype.modify _ { path = path }
  kids' <- ds # traverseWithIndex \i_ d -> do
    let i = Step i_
    kid <- Expr e # getKid_Expr i # fromMaybeM do throw "kid index out of bounds"
    state # updateUiExprViaDiff false (path `List.snoc` i) (Just (Expr e # getElem_UiExpr)) kid d
  pure $ Expr e { kids = kids' }

updateUiExprViaDiff _ path mb_parent (Expr e) (DeleteTooth_Diff i d) state = do
  parent <- mb_parent # fromMaybeM do throw "can't DeleteTooth_Diff at Root"
  -- cleanup all kids around step i
  e.kids # traverseWithIndex_ \i'_ e_kid -> do
    let i' = Step i'_
    when (i /= i') do
      e_kid # cleanup_uiExpr_deep
      Expr e # getElem_UiExpr # removeChild (e_kid # getElem_UiExpr)
  e_kid <- Expr e # getKid_Expr i # fromMaybeM do throw "kid index out of bounds"
  -- replace this expr with the kid at step i
  -- this expr's parent is now the kid's parent
  Expr e # cleanup_UiExpr_shallow
  parent # replaceChild (Expr e # getElem_UiExpr) (e_kid # getElem_UiExpr)
  state # updateUiExprViaDiff true path (pure parent) e_kid d

updateUiExprViaDiff _ path mb_parent e (InsertTooth_Diff (Tooth tooth) d) state = do
  parent <- mb_parent # fromMaybeM do throw "can't InsertTooth_Diff at Root"

  -- replace e with placeholder for now, then replace the placeholder with the
  -- new e' that is rendered from the tooth (which will have e as a child)
  placeholder <- createElement_orphan "div"
  placeholder # setText_Element "{{placeholder}}"
  parent # replaceChild (e # getElem_UiExpr) placeholder

  let kids_L_length = tooth.kids_L # Array.length
  let kids_R_length = tooth.kids_R # Array.length

  e' <- parent # createUiExpr' state path tooth.l
    ({ _L: Step 0, _R: Step $ kids_L_length + kids_R_length } # rangeSteps)
    \i e'_elem -> do
      if i < Step kids_L_length then do
        e'_kid <- tooth.kids_L Array.!! unwrap i # fromMaybeM do throw $ "updateUiExprViaDiff  InsertTooth_Diff: step out-of-bounds: " <> show i
        e'_elem # createUiExpr state (path `List.snoc` i) e'_kid
      else if i == Step kids_L_length then do
        e'_elem # appendChild (e # getElem_UiExpr)
        state # updateUiExprViaDiff true (path `List.snoc` i) (Just e'_elem) e d
      else do
        e'_kid <- tooth.kids_R Array.!! ((-kids_L_length) + (-1) + unwrap i) # fromMaybeM do throw $ "updateUiExprViaDiff  InsertTooth_Diff: step out-of-bounds: " <> show i
        e'_elem # createUiExpr state (path `List.snoc` i) e'_kid

  parent # replaceChild placeholder (e' # getElem_UiExpr)

  pure e'

updateUiExprViaDiff isMoved path mb_parent (Expr e) (ReplaceSpan_Diff j0 j1 span) state = do
  let e_elem = Expr e # getElem_UiExpr
  let e_uiPoints = Expr e # getUiPoints_UiExpr
  let at_diff = Expr e # atIndexSpan_Expr j0 j1
  let
    pre_kids = Array.fold
      [ (at_diff.outside # unwrap).kids_L # map Trident.First
      , span # unwrap # map Trident.Second
      , (at_diff.outside # unwrap).kids_R # map Trident.Third
      ]

  e_elem # removeAllChildren
  -- deep cleanup kids that are permanently removed
  -- add them back appropriately

  -- if (span # offset_Span) == (j1 - j0) then
  --   -- since we're replacing with a span of the same size, don't need to move
  --   -- anything, we just replace the kids appropriately
  --   rangeSteps {_L: ?a, _R: ?a} # traverse
  --   todo ""
  -- else do
  --   kids /\ uiPoints_init <- map Array.unzip $ pre_kids # traverseWithIndex \i_ ->
  --     let
  --       i = Step i_
  --       j = (i # getIndexesAroundStep)._L
  --     in
  --       case _ of
  --         Trident.First kid_L -> todo ""
  --         Trident.Second kid_M -> todo ""
  --         Trident.Third kid_R -> todo ""

  -- pure $ Expr { l: todo "l", kids: todo "kids" }
  todo "updateUiExprViaDiff ... (ReplaceSpan_Diff j0 j1 span) ..."

updateUiExprViaDiff _ path mb_parent e (Replace_Diff e'_) state = do
  parent <- mb_parent # fromMaybeM do throw "can't DeleteTooth_Diff at Root"
  e # cleanup_uiExpr_deep
  e' <- parent # createUiExpr state path e'_
  parent # replaceChild (e # getElem_UiExpr) (e' # getElem_UiExpr)
  pure e'

cleanup_UiExpr_shallow :: UiExpr -> Effect Unit
cleanup_UiExpr_shallow (Expr e) = do
  let elem_e = Expr e # getElem_UiExpr
  -- remove all Expr's EventListeners
  (e.l # unwrap).meta.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # removeEventListener eli
  -- remove all Expr's Point's EventListeners
  (e.l # unwrap).meta.uiPoints # traverse_ cleanup_UiPoint

cleanup_uiExpr_deep :: UiExpr -> Effect Unit
cleanup_uiExpr_deep (Expr e) = do
  let elem_e = Expr e # getElem_UiExpr
  -- remove all Expr's EventListeners
  (e.l # unwrap).meta.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # removeEventListener eli
  -- remove all Expr's Point's EventListeners
  (e.l # unwrap).meta.uiPoints # traverse_ cleanup_UiPoint
  e.kids # traverse_ cleanup_uiExpr_deep

cleanup_UiPoint :: UiPoint -> Effect Unit
cleanup_UiPoint uiPoint = do
  uiPoint.eventListenerInfos # traverse_ \eli -> do
    uiPoint.elem # Element.toEventTarget # removeEventListener eli

--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

insertSpan :: State -> Point -> Span PureLabel -> Effect Unit
insertSpan state p (Span s) = do
  uiExprRoot <- state # getUiExpr_root
  let at_h = uiExprRoot # atPoint p
  todo "insertSpan"

insertZipper :: State -> SpanH -> Zipper PureLabel -> Effect Unit
insertZipper state (SpanH h) (Zipper z) = do
  uiExprRoot <- state # getUiExpr_root
  let at_h = uiExprRoot # atSpan (SpanH h)
  todo "insertZipper"

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

getUiExpr_root :: State -> Effect UiExpr
getUiExpr_root state = state.mb_uiExprRoot # Ref.read >>= case _ of
  Nothing -> throw $ "getUiExpr_root: isNothing state.expr!"
  Just expr -> pure expr

getUiExpr :: State -> Path -> Effect UiExpr
getUiExpr state path = do
  uiExprRoot <- state # getUiExpr_root
  pure (uiExprRoot # atSubExpr path).here

getUiPoint :: State -> Point -> Effect UiPoint
getUiPoint state (Point p) = do
  uiExprRoot <- state # getUiExpr_root
  let expr = (uiExprRoot # atSubExpr p.path).here
  ((expr # unwrap).l # unwrap).meta.uiPoints Array.!! (unwrap p.j)
    # fromMaybeM (throw "getUiPoint: p.j out-of-bounds")

