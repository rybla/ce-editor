module Ui.App where

import Prelude

import Data.Array as Array
import Data.Expr (Diff(..), Expr(..), Handle(..), Index(..), Path, Point(..), Span(..), SpanFocus(..), SpanH(..), Step(..), Tooth(..), Zipper(..), ZipperFocus(..), atPoint, atSpan, atSubExpr, defaultHandle, getEndPoints_SpanH, getEndPoints_ZipperH, getExtremeSteps, getIndexesAroundStep, getKid_Expr, mkExpr, stepsRange)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Move as Expr.Move
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor.Example.Editor1 (Dat(..), L(..), mkL)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Ui.Common (EventListenerInfo, KeyInfo(..), addClass, addEventListenerWithOptions, appendChild, body, createElement, createElement_orphan, doc, fromEventToKeyInfo, matchKeyInfo, matchMapKeyInfo, removeChild, removeClass, removeEventListener, replaceChild, setText_Element, shiftKey)
import Utility (fromMaybeM, todo, (:%=), (:=))
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
-- types
--------------------------------------------------------------------------------

type State =
  { mb_uiExprRoot :: Ref (Maybe UiExpr)
  , mb_handle :: Ref (Maybe Handle)
  , mb_dragOrigin :: Ref (Maybe Handle)
  }

type PureLabel = L {}

mkPureExpr s kids = mkExpr (L { dat: String s, meta: {} }) kids

infix 4 mkPureExpr as %

type UiLabel = L UiExprMeta

type PureExpr = Expr PureLabel
type UiExpr = Expr UiLabel

getElem_UiExpr :: UiExpr -> Element
getElem_UiExpr e = ((e # unwrap).l # unwrap).meta.elem

type UiEditor =
  { elem :: Element
  , eventListenerInfos :: Array EventListenerInfo
  }

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

  expr' <- elem # renderExpr state Nil expr
  state.mb_uiExprRoot := pure expr'

  eventListenerInfos <- sequence
    [ state # eventListenerInfo_stopDrag
    , state # eventListenerInfo_keyboardAction
    ]

  pure { elem, eventListenerInfos }

newState :: Effect State
newState = do
  mb_uiExprRoot <- Ref.new Nothing
  mb_handle <- Ref.new Nothing
  mb_dragOrigin <- Ref.new Nothing
  pure { mb_uiExprRoot, mb_handle, mb_dragOrigin }

eventListenerInfo_stopDrag :: State -> Effect EventListenerInfo
eventListenerInfo_stopDrag state = doc # Document.toEventTarget
  # addEventListenerWithOptions (EventType "mouseup") { capture: true, passive: true } \_event -> do
      state.mb_dragOrigin := none

eventListenerInfo_keyboardAction :: State -> Effect EventListenerInfo
eventListenerInfo_keyboardAction state = doc # Document.toEventTarget
  # addEventListenerWithOptions (EventType "keydown") { capture: true } \event -> do
      let KeyInfo ki = event # fromEventToKeyInfo
      case unit of
        -- move
        _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just false, alt: Just false } -> do
          event # preventDefault
          state.mb_handle # Ref.read >>= case _ of
            Nothing -> state # setHandle (Just defaultHandle)
            Just handle -> do
              uiExprRoot <- state # getUiExpr_root
              case Expr.Move.move uiExprRoot dir handle of
                Nothing -> pure unit
                Just point -> state # setHandle (Just (Point_Handle point))
        -- drag move
        _ | Just dir <- KeyInfo ki # matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just true, alt: Just false } -> do
          event # preventDefault
          state.mb_handle # Ref.read >>= case _ of
            Nothing -> state # setHandle (Just defaultHandle)
            Just handle -> do
              uiExprRoot <- state # getUiExpr_root
              case Expr.Move.moveUntil uiExprRoot dir handle (\p -> Expr.Drag.drag handle p uiExprRoot) of
                Nothing -> pure unit
                Just handle' -> state # setHandle (Just handle')
        -- test 1
        _ | KeyInfo ki # matchKeyInfo (_ == "1") { cmd: pure true } -> do
          event # preventDefault
          state.mb_dragOrigin := none
          state # setHandle Nothing
          state # applyDiff
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
          state # applyDiff
            ( Inject_Diff {- Root -}
                [ Replace_Diff (mkPureExpr "D" [])
                ]
            )
        -- test 3
        _ | KeyInfo ki # matchKeyInfo (_ == "3") { cmd: pure true } -> do
          event # preventDefault
          state.mb_dragOrigin := none
          state # setHandle Nothing
          state # applyDiff
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

--------------------------------------------------------------------------------
-- renderExpr
--------------------------------------------------------------------------------

renderExpr :: State -> Path -> PureExpr -> Element -> Effect UiExpr
renderExpr state path (Expr expr) parent = do
  Console.log $ "renderExpr: " <> show (Expr expr)
  parent # renderExpr' state path expr.l (Expr expr # getExtremeSteps) \i ->
    renderExpr state (i : path) (Expr expr # getKid_Expr i)

renderExpr' :: State -> Path -> PureLabel -> Maybe { _L :: Step, _R :: Step } -> (Step -> Element -> Effect UiExpr) -> Element -> Effect UiExpr
renderExpr' state path0 label extremeSteps renderKid elem_parent = do
  pathRef <- Ref.new path0
  -- create element
  elem_expr <- elem_parent # createElement "div"

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
  kids' /\ uiPoints_init <- case extremeSteps <#> stepsRange of
    Nothing -> pure $ [] /\ []
    Just steps -> map Array.unzip $ steps # traverse \i -> do
      let j = (i # getIndexesAroundStep)._L
      uiPoint <- elem_expr # renderPoint state (Point { path: path0, j })
      kid' <- elem_expr # renderKid i
      pure $ kid' /\ uiPoint
  uiPoint_last <- elem_expr # renderPoint state
    ( Point
        { path: path0
        , j: extremeSteps # maybe (Index 0) (\step -> (step._R # getIndexesAroundStep)._R)
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
            , path: pathRef
            , uiPoints
            }
        }
    , kids: kids'
    }

--------------------------------------------------------------------------------
-- renderPoint
--------------------------------------------------------------------------------

renderPoint :: State -> Point -> Element -> Effect UiPoint
renderPoint state (Point point0) elem_parent = do
  elem <- elem_parent # createElement "div"
  elem # addClass "Point"

  pointRef <- Ref.new (Point point0)

  eventListenerInfos <- sequence
    [ elem # Element.toEventTarget # addEventListenerWithOptions (EventType "mousedown") { capture: true, passive: true } \event -> do
        Point point <- pointRef # Ref.read
        -- Console.log $ "Point: mousedown" <> show (Point point)
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
        state.mb_dragOrigin # Ref.read >>= case _ of
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

applyDiff :: Diff PureLabel -> State -> Effect Unit
applyDiff diff0 state = do
  uiExprRoot <- state # getUiExpr_root
  uiExprRoot' <- go Nil Nothing uiExprRoot diff0
  state.mb_uiExprRoot := pure uiExprRoot'
  where
  go :: Path -> Maybe Element -> UiExpr -> Diff PureLabel -> Effect UiExpr

  go path _ (Expr e) (Inject_Diff ds) = do
    (e.l # unwrap).meta.path := path
    (e.l # unwrap).meta.uiPoints # traverse_ \uiPoint -> uiPoint.point :%= Newtype.modify _ { path = path }
    kids' <- ds # traverseWithIndex \i_ d -> do
      let i = Step i_
      go (i : path) (Just (Expr e # getElem_UiExpr)) (Expr e # getKid_Expr i) d
    pure $ Expr e { kids = kids' }

  go path mb_parent (Expr e) (DeleteTooth_Diff i d) = do
    parent <- mb_parent # fromMaybeM do throw "can't DeleteTooth_Diff at Root"
    -- cleanup all kids around step i
    e.kids # traverseWithIndex_ \i'_ e_kid -> do
      let i' = Step i'_
      when (i /= i') do
        e_kid # cleanup_uiExpr_deep
        Expr e # getElem_UiExpr # removeChild (e_kid # getElem_UiExpr)
    let e_kid = Expr e # getKid_Expr i
    -- replace this expr with the kid at step i
    -- this expr's parent is now the kid's parent
    Expr e # cleanup_UiExpr_shallow
    parent # replaceChild (Expr e # getElem_UiExpr) (e_kid # getElem_UiExpr)
    go path (pure parent) e_kid d

  go path mb_parent e (InsertTooth_Diff (Tooth tooth) d) = do
    parent <- mb_parent # fromMaybeM do throw "can't InsertTooth_Diff at Root"

    -- replace e with placeholder for now, then replace the placeholder with the
    -- new e' that is rendered from the tooth (which will have e as a child)
    placeholder <- createElement_orphan "div"
    placeholder # setText_Element "{{placeholder}}"
    parent # replaceChild (e # getElem_UiExpr) placeholder

    let kids_L_length = tooth.kids_L # Array.length
    let kids_R_length = tooth.kids_R # Array.length

    e' <- parent # renderExpr' state path tooth.l
      (Just { _L: Step 0, _R: Step (kids_L_length + kids_R_length) })
      \i e'_elem -> do
        Console.log $ "i = " <> show i
        if i < Step kids_L_length then do
          e'_kid <- tooth.kids_L Array.!! unwrap i # fromMaybeM do throw $ "go InsertTooth_Diff: step out-of-bounds: " <> show i
          e'_elem # renderExpr state (i : path) e'_kid
        else if i == Step kids_L_length then do
          e'_elem # appendChild (e # getElem_UiExpr)
          go (i : path) (Just e'_elem) e d
        else do
          e'_kid <- tooth.kids_R Array.!! ((-kids_L_length) + (-1) + unwrap i) # fromMaybeM do throw $ "go InsertTooth_Diff: step out-of-bounds: " <> show i
          e'_elem # renderExpr state (i : path) e'_kid

    parent # replaceChild placeholder (e' # getElem_UiExpr)

    pure e'

  go path mb_parent e (Replace_Diff e'_) = do
    parent <- mb_parent # fromMaybeM do throw "can't DeleteTooth_Diff at Root"
    e # cleanup_uiExpr_deep
    e' <- parent # renderExpr state path e'_
    parent # replaceChild (e # getElem_UiExpr) (e' # getElem_UiExpr)
    pure e'

cleanup_UiExpr_shallow :: UiExpr -> Effect Unit
cleanup_UiExpr_shallow (Expr e) = do
  let elem_e = Expr e # getElem_UiExpr
  -- remove all Expr's EventListeners
  (e.l # unwrap).meta.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # removeEventListener eli
  -- remove all Expr's Point's EventListeners
  (e.l # unwrap).meta.uiPoints # traverse_ \uiPoint -> uiPoint.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # removeEventListener eli

cleanup_uiExpr_deep :: UiExpr -> Effect Unit
cleanup_uiExpr_deep (Expr e) = do
  let elem_e = Expr e # getElem_UiExpr
  -- remove all Expr's EventListeners
  (e.l # unwrap).meta.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # removeEventListener eli
  -- remove all Expr's Point's EventListeners
  (e.l # unwrap).meta.uiPoints # traverse_ \uiPoint -> uiPoint.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # removeEventListener eli
  e.kids # traverse_ cleanup_uiExpr_deep

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
  pure (uiExprRoot # atSubExpr path).at

getUiPoint :: State -> Point -> Effect UiPoint
getUiPoint state (Point p) = do
  uiExprRoot <- state # getUiExpr_root
  let expr = (uiExprRoot # atSubExpr p.path).at
  ((expr # unwrap).l # unwrap).meta.uiPoints Array.!! (unwrap p.j)
    # fromMaybeM (throw "getUiPoint: p.j out-of-bounds")

