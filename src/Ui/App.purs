module Ui.App where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Expr (Diff(..), Expr(..), Handle(..), Path, Point(..), Span(..), SpanFocus(..), SpanH(..), Step(..), Tooth(..), Zipper(..), ZipperFocus(..), atIndexSpan_Expr, atInjectDiff, atPoint, atSpan, atSubExpr, defaultHandle, getEndPoints_SpanH, getEndPoints_ZipperH, getFocusPoint, getIndexesAroundStep, getKid_Expr, mkExpr, offset_Span, rangeKidSteps, rangeSteps, toNePath)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Move as Expr.Move
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Trident as Trident
import Data.Unfoldable (none)
import Editor.Example.Editor1 (Dat(..), L(..), mkL)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prim.Row (class Nub, class Union)
import Ui.Common as Ui.Common
import Ui.Element as Element
import Ui.Event (KeyInfo(..))
import Ui.Event as Event
import Utility (fromMaybeM, isAlpha, todo, (:%=), (:=))
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (EventTarget)

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
  void $ Element.body # createEditor

--------------------------------------------------------------------------------
-- Ctx
--------------------------------------------------------------------------------

type Ctx =
  { mb_uiExprRoot :: Ref (Maybe UiExpr)
  , mb_handle :: Ref (Maybe Handle)
  , mb_dragOrigin :: Ref (Maybe Handle)
  }

newCtx :: Effect Ctx
newCtx = do
  mb_uiExprRoot <- Ref.new Nothing
  mb_handle <- Ref.new Nothing
  mb_dragOrigin <- Ref.new Nothing
  pure { mb_uiExprRoot, mb_handle, mb_dragOrigin }

--------------------------------------------------------------------------------
-- UiEditor
--------------------------------------------------------------------------------

type UiEditor =
  { elem :: Element
  , eventListenerInfos :: Array Event.EventListenerInfo
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
  , eventListenerInfos :: Array Event.EventListenerInfo
  , path :: Ref Path
  -- indexed by each Point Index
  , uiPoints :: Array UiPoint
  }

type UiPoint =
  { elem :: Element
  , eventListenerInfos :: Array Event.EventListenerInfo
  , point :: Ref Point
  }

getElem_UiExpr :: UiExpr -> Element
getElem_UiExpr e = ((e # unwrap).l # unwrap).meta.elem

getUiPoints_UiExpr :: UiExpr -> Array UiPoint
getUiPoints_UiExpr e = ((e # unwrap).l # unwrap).meta.uiPoints

--------------------------------------------------------------------------------
-- createEditor
--------------------------------------------------------------------------------

createEditor :: Element -> Effect UiEditor
createEditor parent = do
  let expr_ = mkExpr (mkL Root {}) [ config.initialExpr ]
  Console.logShow expr_

  ctx <- newCtx

  elem <- parent # Element.createChild "div"
  elem # Element.addClass "Editor"

  expr <- flip runReaderT ctx $ createUiExpr Nil expr_
  elem # Element.appendChild (expr # getElem_UiExpr)
  ctx.mb_uiExprRoot := pure expr

  eventListenerInfos <- flip runReaderT ctx $ sequence
    [ eventListenerInfo_stopDrag_Editor
    , eventListenerInfo_keydown_Editor
    ]

  pure { elem, eventListenerInfos }

eventListenerInfo_stopDrag_Editor :: EditorM Event.EventListenerInfo
eventListenerInfo_stopDrag_Editor = Element.doc # Document.toEventTarget # addEventListenerWithOptions (EventType "mouseup") { capture: true, passive: true } \_event -> do
  ctx <- ask
  lift $ ctx.mb_dragOrigin := none

eventListenerInfo_keydown_Editor :: EditorM Event.EventListenerInfo
eventListenerInfo_keydown_Editor = Element.doc # Document.toEventTarget # addEventListenerWithOptions (EventType "keydown") { capture: true } \event -> do
  ctx <- ask
  let KeyInfo ki = event # Event.fromEventToKeyInfo
  -- Console.log $ "keydown: " <> ki.key
  mb_dragOrigin <- lift $ ctx.mb_dragOrigin # Ref.read
  mb_handle <- lift $ ctx.mb_handle # Ref.read
  case unit of
    -- move
    _ | Just dir <- KeyInfo ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just false, alt: Just false } -> do
      lift $ event # preventDefault
      lift $ ctx.mb_dragOrigin := none
      case mb_handle of
        Nothing -> do
          setHandle (Just defaultHandle)
        Just handle -> do
          uiExprRoot <- getUiExpr_root
          case Expr.Move.movePoint uiExprRoot dir (handle # getFocusPoint) of
            Nothing -> setHandle (Just (Point_Handle (handle # getFocusPoint)))
            Just point -> setHandle (Just (Point_Handle point))
    -- drag move
    _ | Just dir <- KeyInfo ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just true, alt: Just false } -> do
      Console.log "drag move"
      lift $ event # preventDefault
      case mb_handle of
        Nothing -> do
          -- initialize dragOrigin
          case mb_dragOrigin of
            Nothing -> do
              lift $ ctx.mb_dragOrigin := pure defaultHandle
            Just _ -> do
              pure unit
          setHandle (Just defaultHandle)
        Just handle -> do
          uiExprRoot <- getUiExpr_root
          -- initialize dragOrigin
          dragOrigin <- case mb_dragOrigin of
            Nothing -> do
              Console.log "initialize dragOrigin"
              lift $ ctx.mb_dragOrigin := pure handle
              pure handle
            Just dragOrigin -> do
              pure dragOrigin
          case Expr.Move.movePointUntil uiExprRoot dir (handle # getFocusPoint) \p -> uiExprRoot # Expr.Drag.drag dragOrigin p of
            Nothing -> pure unit
            Just handle' -> do
              setHandle (Just handle')
    -- move handle focus
    _ | Just dir <- KeyInfo ki # Event.matchMapKeyInfo Expr.Move.fromKeyToDir { cmd: Just false, shift: Just false, alt: Just true } -> do
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          setHandle (Just (handle # Expr.Move.moveHandleFocus dir))
    -- insert
    _ | KeyInfo ki # Event.matchKeyInfo isAlpha { cmd: pure false, alt: pure false } -> do
      case mb_handle of
        Nothing -> pure unit
        Just handle -> do
          expr <- getUiExpr_root
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
                  setHandle none
                  updateUiExprViaDiff_root diff
            _ -> pure unit -- TODO: other kinds of insert
          pure unit
    -- test 1
    _ | KeyInfo ki # Event.matchKeyInfo (_ == "1") { cmd: pure true } -> do
      lift $ event # preventDefault
      lift $ ctx.mb_dragOrigin := none
      setHandle Nothing
      updateUiExprViaDiff_root
        ( Inject_Diff {- Root -}
            [ DeleteTooth_Diff {- A -}  (Step 0)
                (Inject_Diff {- C -}  [])
            ]
        )
    -- test 2
    _ | KeyInfo ki # Event.matchKeyInfo (_ == "2") { cmd: pure true } -> do
      lift $ event # preventDefault
      lift $ ctx.mb_dragOrigin := none
      setHandle Nothing
      updateUiExprViaDiff_root
        ( Inject_Diff {- Root -}
            [ Replace_Diff (mkPureExpr "D" [])
            ]
        )
    -- test 3
    _ | KeyInfo ki # Event.matchKeyInfo (_ == "3") { cmd: pure true } -> do
      lift $ event # preventDefault
      lift $ ctx.mb_dragOrigin := none
      setHandle Nothing
      updateUiExprViaDiff_root
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

eventListenerInfo_keyup_Editor :: EditorM Event.EventListenerInfo
eventListenerInfo_keyup_Editor = Element.doc # Document.toEventTarget # addEventListenerWithOptions (EventType "keyup") { capture: true } \event -> do
  -- let KeyInfo ki = event # fromEventToKeyInfo
  -- mb_handle <- ctx.mb_handle # Ref.read
  -- mb_dragOrigin <- ctx.mb_dragOrigin # Ref.read
  case unit of
    _ -> pure unit
  pure unit

--------------------------------------------------------------------------------
-- EditorM
--------------------------------------------------------------------------------

type EditorM = ReaderT Ctx Effect

--------------------------------------------------------------------------------
-- createUiExpr
--------------------------------------------------------------------------------

createUiExpr :: Path -> PureExpr -> EditorM UiExpr
createUiExpr path (Expr expr) = do
  Console.log $ "createUiExpr: " <> show (Expr expr)

  createUiExpr' path expr.l (Expr expr # rangeKidSteps) \i -> do
    kid <- Expr expr # getKid_Expr i # fromMaybeM do lift $ throw $ "kid index out of bounds"
    createUiExpr (path `List.snoc` i) kid

createUiExpr' :: Path -> PureLabel -> Array Step -> (Step -> EditorM UiExpr) -> EditorM UiExpr
createUiExpr' path0 label steps renderKid = do
  path_ref <- lift $ Ref.new path0

  elem_expr <- lift $ Element.create "div"
  uiExprs_kids <- steps # traverse renderKid

  assembleUiExpr path_ref elem_expr label uiExprs_kids

-- | Given these components of a UiExpr
-- |  - path_ref :: Ref Path
-- |  - elem_expr :: Element
-- |  - label :: PureLabel
-- |  - kids :: Array UiExpr
-- | assembles the UiExpr on the elem_expr and returns the resulting UiExpr.
assembleUiExpr :: Ref Path -> Element -> PureLabel -> Array UiExpr -> EditorM UiExpr
assembleUiExpr path_ref elem_expr label kids = do
  path <- lift $ path_ref # Ref.read

  -- attributes
  lift $ elem_expr # Element.addClass "Expr"
  case config.displayStyle of
    Inline_DisplayStyle -> lift $ elem_expr # Element.addClass "Inline"
    Nested_DisplayStyle -> lift $ elem_expr # Element.addClass "Nested"
  case label of
    L { dat: Root } -> lift $ elem_expr # Element.addClass "Root"
    _ -> pure unit

  eventListenerInfos <- lift $ sequence
    []

  -- open
  when (config.displayStyle == Inline_DisplayStyle) do
    elem_open <- lift $ elem_expr # Element.createChild "div"
    lift $ elem_open # Element.addClass "Punctuation"
    lift $ elem_open # Element.toNode # Node.setTextContent case label of
      L { dat: Root } -> ""
      _ -> "("

  -- label
  do
    elem_label <- lift $ elem_expr # Element.createChild "div"
    lift $ elem_label # Element.addClass "Label"
    lift $ elem_label # Element.toNode # Node.setTextContent (show label)

  -- kids
  uiPoints_init <- kids # traverseWithIndex \i_ uiExpr_kid -> do
    let i = Step i_
    let j = (i # getIndexesAroundStep)._L
    uiPoint <- createUiPoint (Point { path, j })
    lift $ elem_expr # Element.appendChild uiPoint.elem
    lift $ elem_expr # Element.appendChild (uiExpr_kid # getElem_UiExpr)
    pure uiPoint
  uiPoint_last <- createUiPoint $
    Point
      { path
      , j: kids # Span # offset_Span
      }
  lift $ elem_expr # Element.appendChild uiPoint_last.elem
  let uiPoints = uiPoints_init `Array.snoc` uiPoint_last

  -- close
  when (config.displayStyle == Inline_DisplayStyle) do
    elem_close <- lift $ elem_expr # Element.createChild "div"
    lift $ elem_close # Element.addClass "Punctuation"
    lift $ elem_close # Element.toNode # Node.setTextContent case label of
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

--------------------------------------------------------------------------------
-- createUiPoint
--------------------------------------------------------------------------------

createUiPoint :: Point -> EditorM UiPoint
createUiPoint (Point point0) = do
  ctx <- ask
  Console.log $ "createUiPoint " <> show (Point point0)

  pointRef <- lift $ Ref.new (Point point0)

  elem <- lift $ Element.create "div"
  lift $ elem # Element.addClass "Point"

  eventListenerInfos <- sequence
    [ elem # Element.toEventTarget # addEventListenerWithOptions (EventType "mousedown") { capture: true, passive: true } \event -> do
        Point point <- lift $ pointRef # Ref.read
        (lift $ ctx.mb_handle # Ref.read) >>= case _ of
          Just h | event # Event.shiftKey -> do
            expr <- getUiExpr_root
            lift $ ctx.mb_dragOrigin := pure h
            case Expr.Drag.drag h (Point point) expr of
              Nothing -> pure unit
              Just h' -> setHandle (pure h')
          Just h -> do
            uiExprRoot <- getUiExpr_root
            let dragOrigin = Expr.Drag.getDragOrigin h (Point point)
            lift $ ctx.mb_dragOrigin := pure dragOrigin
            case Expr.Drag.drag dragOrigin (Point point) uiExprRoot of
              Nothing -> pure unit
              Just h' -> setHandle (pure h')
          _ -> do
            let h' = Point_Handle (Point point)
            lift $ ctx.mb_dragOrigin := pure h'
            setHandle (pure h')
    , elem # Element.toEventTarget # addEventListenerWithOptions (EventType "mouseenter") { capture: true, passive: true } \_event -> do
        Point point <- lift $ pointRef # Ref.read
        -- Console.log $ "Point: mouseenter" <> show (Point point)
        mb_dragOrigin <- lift $ ctx.mb_dragOrigin # Ref.read
        case mb_dragOrigin of
          Nothing -> pure unit
          Just h -> do
            uiExprRoot <- getUiExpr_root
            case Expr.Drag.drag h (Point point) uiExprRoot of
              Nothing -> pure unit
              Just h' -> setHandle (pure h')
    ]

  lift do
    elem_Focus <- elem # Element.createChild "div"
    elem_Focus # Element.addClass "Focus"

    elem_L <- elem # Element.createChild "div"
    elem_L # Element.addClass "Left"

    elem_M <- elem # Element.createChild "div"
    elem_M # Element.addClass "Middle"

    elem_R <- elem # Element.createChild "div"
    elem_R # Element.addClass "Right"

  pure
    { elem
    , eventListenerInfos
    , point: pointRef
    }

--------------------------------------------------------------------------------
-- Diff
--------------------------------------------------------------------------------

updateUiExprViaDiff_root :: Diff PureLabel -> EditorM Unit
updateUiExprViaDiff_root diff0 = do
  ctx <- ask
  uiExprRoot <- getUiExpr_root
  uiExprRoot' <- updateUiExprViaDiff false Nil (Just (uiExprRoot # getElem_UiExpr)) uiExprRoot diff0
  lift $ ctx.mb_uiExprRoot := pure uiExprRoot'

-- | Updates the UiExpr and all of its descendants, which updates each UiExpr's
-- | ctx and Ui elements.
updateUiExpr :: Path -> UiExpr -> EditorM UiExpr
updateUiExpr path (Expr e) = do
  lift $ (e.l # unwrap).meta.path := path
  lift $ (e.l # unwrap).meta.uiPoints # traverse_ \uiPoint ->
    uiPoint.point :%= Newtype.modify _ { path = path }
  kids' <- e.kids # traverseWithIndex \i_ kid -> do
    let i = Step i_
    updateUiExpr (path `List.snoc` i) kid
  pure $ Expr e { kids = kids' }

-- | Updates the UiExpr and all of its descendants via a Diff, which applies the
-- | Diff's edit to the Expr and updates Ui elements accordingly.
updateUiExprViaDiff :: Boolean -> Path -> Maybe Element -> UiExpr -> Diff PureLabel -> EditorM UiExpr

updateUiExprViaDiff isMoved path _mb_parent expr Id_Diff = do
  -- only need to update if has moved
  if not isMoved then pure expr
  else do
    updateUiExpr path expr

updateUiExprViaDiff _ path _mb_parent (Expr e) (Inject_Diff ds) = do
  lift $ (e.l # unwrap).meta.path := path
  lift $ (e.l # unwrap).meta.uiPoints # traverse_ \uiPoint ->
    uiPoint.point :%= Newtype.modify _ { path = path }
  kids' <- ds # traverseWithIndex \i_ d -> do
    let i = Step i_
    kid <- Expr e # getKid_Expr i # fromMaybeM do lift $ throw "kid index out of bounds"
    updateUiExprViaDiff false (path `List.snoc` i) (Just (Expr e # getElem_UiExpr)) kid d
  pure $ Expr e { kids = kids' }

updateUiExprViaDiff _ path mb_parent (Expr e) (DeleteTooth_Diff i d) = do
  parent <- mb_parent # fromMaybeM do lift $ throw "can't DeleteTooth_Diff at Root"
  -- cleanup all kids around step i
  e.kids # traverseWithIndex_ \i'_ e_kid -> do
    let i' = Step i'_
    when (i /= i') do
      lift $ e_kid # cleanup_uiExpr_deep
      lift $ Expr e # getElem_UiExpr # Element.removeChild (e_kid # getElem_UiExpr)
  e_kid <- Expr e # getKid_Expr i # fromMaybeM do lift $ throw "kid index out of bounds"
  -- replace this expr with the kid at step i
  -- this expr's parent is now the kid's parent
  lift $ Expr e # cleanup_UiExpr_shallow
  lift $ parent # Element.replaceChild (Expr e # getElem_UiExpr) (e_kid # getElem_UiExpr)
  updateUiExprViaDiff true path (Just parent) e_kid d

updateUiExprViaDiff _ path mb_parent e (InsertTooth_Diff (Tooth tooth) d) = do
  parent <- mb_parent # fromMaybeM do lift $ throw "can't InsertTooth_Diff at Root"

  -- replace e with placeholder for now, then replace the placeholder with the
  -- new e' that is rendered from the tooth (which will have e as a child)
  e'_placeholder <- lift $ Element.create "div"
  lift $ e'_placeholder # Element.setText "{{e'_placeholder}}"
  lift $ parent # Element.replaceChild (e # getElem_UiExpr) e'_placeholder

  let kids_L_length = tooth.kids_L # Array.length
  let kids_R_length = tooth.kids_R # Array.length

  e' <- createUiExpr' path tooth.l
    ({ _L: Step 0, _R: Step $ kids_L_length + kids_R_length } # rangeSteps)
    \i -> do
      if i < Step kids_L_length then do
        kid <- tooth.kids_L Array.!! unwrap i # fromMaybeM do lift $ throw $ "updateUiExprViaDiff InsertTooth_Diff: step out-of-bounds: " <> show i
        uiExpr_kid <- createUiExpr (path `List.snoc` i) kid
        pure uiExpr_kid
      else if i == Step kids_L_length then do
        -- this placeholder doesn't matter since the result of
        -- `updateUiExprViaDiff` will be appended to `e` anyway
        e_parent_placeholder <- lift $ Element.create "div"
        lift $ e_parent_placeholder # Element.setText "{{e_parent_placeholder}}"
        updateUiExprViaDiff true (path `List.snoc` i) (Just e_parent_placeholder) e d
      else do
        kid <- tooth.kids_R Array.!! ((-kids_L_length) + (-1) + unwrap i) # fromMaybeM do lift $ throw $ "updateUiExprViaDiff InsertTooth_Diff: step out-of-bounds: " <> show i
        uiExpr_kid <- createUiExpr (path `List.snoc` i) kid
        pure uiExpr_kid

  lift $ parent # Element.replaceChild e'_placeholder (e' # getElem_UiExpr)

  pure e'

updateUiExprViaDiff isMoved path mb_parent (Expr e) (ReplaceSpan_Diff j0 j1 span) = do
  let e_elem = Expr e # getElem_UiExpr
  let e_uiPoints = Expr e # getUiPoints_UiExpr
  let at_diff = Expr e # atIndexSpan_Expr j0 j1
  let
    pre_kids = Array.fold
      [ (at_diff.outside # unwrap).kids_L # map Trident.First
      , span # unwrap # map Trident.Second
      , (at_diff.outside # unwrap).kids_R # map Trident.Third
      ]

  lift $ e_elem # Element.removeAllChildren
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

updateUiExprViaDiff _ path mb_parent e (Replace_Diff e'_) = do
  parent <- mb_parent # fromMaybeM do lift $ throw "can't DeleteTooth_Diff at Root"
  lift $ e # cleanup_uiExpr_deep
  e' <- createUiExpr path e'_
  lift $ parent # Element.replaceChild (e # getElem_UiExpr) (e' # getElem_UiExpr)
  pure e'

cleanup_UiExpr_shallow :: UiExpr -> Effect Unit
cleanup_UiExpr_shallow (Expr e) = do
  let elem_e = Expr e # getElem_UiExpr
  -- remove all Expr's EventListeners
  (e.l # unwrap).meta.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # Event.removeEventListener eli
  -- remove all Expr's Point's EventListeners
  (e.l # unwrap).meta.uiPoints # traverse_ cleanup_UiPoint

cleanup_uiExpr_deep :: UiExpr -> Effect Unit
cleanup_uiExpr_deep (Expr e) = do
  let elem_e = Expr e # getElem_UiExpr
  -- remove all Expr's EventListeners
  (e.l # unwrap).meta.eventListenerInfos # traverse_ \eli -> do
    elem_e # Element.toEventTarget # Event.removeEventListener eli
  -- remove all Expr's Point's EventListeners
  (e.l # unwrap).meta.uiPoints # traverse_ cleanup_UiPoint
  e.kids # traverse_ cleanup_uiExpr_deep

cleanup_UiPoint :: UiPoint -> Effect Unit
cleanup_UiPoint uiPoint = do
  uiPoint.eventListenerInfos # traverse_ \eli -> do
    uiPoint.elem # Element.toEventTarget # Event.removeEventListener eli

--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

insertSpan :: Point -> Span PureLabel -> EditorM Unit
insertSpan p (Span s) = do
  uiExprRoot <- getUiExpr_root
  let at_h = uiExprRoot # atPoint p
  todo "insertSpan"

insertZipper :: SpanH -> Zipper PureLabel -> EditorM Unit
insertZipper (SpanH h) (Zipper z) = do
  uiExprRoot <- getUiExpr_root
  let at_h = uiExprRoot # atSpan (SpanH h)
  todo "insertZipper"

modifyHandle :: Boolean -> Maybe Handle -> EditorM Unit
modifyHandle b mb_handle = do

  let modifyClass = if b then Element.addClass else Element.removeClass
  case mb_handle of
    Nothing -> pure unit
    Just (Point_Handle p) -> do
      uiPoint <- getUiPoint p
      lift $ uiPoint.elem # modifyClass "Point_Handle"
      lift $ uiPoint.elem # modifyClass "HandleFocus"
    Just (SpanH_Handle h f) -> do
      let p = getEndPoints_SpanH h
      uiPoint_L <- getUiPoint p._L
      lift $ uiPoint_L.elem # modifyClass "SpanH_Handle_Left"
      uiPoint_R <- getUiPoint p._R
      lift $ uiPoint_R.elem # modifyClass "SpanH_Handle_Right"
      case f of
        Left_SpanFocus -> lift $ uiPoint_L.elem # modifyClass "HandleFocus"
        Right_SpanFocus -> lift $ uiPoint_R.elem # modifyClass "HandleFocus"
    Just (ZipperH_Handle h f) -> do
      let p = getEndPoints_ZipperH h
      uiPoint_OL <- getUiPoint p._OL
      lift $ uiPoint_OL.elem # modifyClass "ZipperH_Handle_OuterLeft"
      uiPoint_IL <- getUiPoint p._IL
      lift $ uiPoint_IL.elem # modifyClass "ZipperH_Handle_InnerLeft"
      uiPoint_IR <- getUiPoint p._IR
      lift $ uiPoint_IR.elem # modifyClass "ZipperH_Handle_InnerRight"
      uiPoint_OR <- getUiPoint p._OR
      lift $ uiPoint_OR.elem # modifyClass "ZipperH_Handle_OuterRight"
      case f of
        OuterLeft_ZipperFocus -> lift $ uiPoint_OL.elem # modifyClass "HandleFocus"
        InnerLeft_ZipperFocus -> lift $ uiPoint_IL.elem # modifyClass "HandleFocus"
        InnerRight_ZipperFocus -> lift $ uiPoint_IR.elem # modifyClass "HandleFocus"
        OuterRight_ZipperFocus -> lift $ uiPoint_OR.elem # modifyClass "HandleFocus"

setHandle :: Maybe Handle -> EditorM Unit
setHandle mb_handle' = do
  ctx <- ask
  mb_handle <- lift $ ctx.mb_handle # Ref.read
  modifyHandle false mb_handle
  modifyHandle true mb_handle'
  lift $ ctx.mb_handle := mb_handle'

getUiExpr_root :: EditorM UiExpr
getUiExpr_root = do
  ctx <- ask
  (lift $ ctx.mb_uiExprRoot # Ref.read) >>= case _ of
    Nothing -> lift $ throw $ "getUiExpr_root: isNothing ctx.expr!"
    Just expr -> pure expr

getUiExpr :: Path -> EditorM UiExpr
getUiExpr path = do
  uiExprRoot <- getUiExpr_root
  pure (uiExprRoot # atSubExpr path).here

getUiPoint :: Point -> EditorM UiPoint
getUiPoint (Point p) = do
  uiExprRoot <- getUiExpr_root
  let expr = (uiExprRoot # atSubExpr p.path).here
  ((expr # unwrap).l # unwrap).meta.uiPoints Array.!! (unwrap p.j)
    # fromMaybeM (lift $ throw "getUiPoint: p.j out-of-bounds")

--------------------------------------------------------------------------------
-- aliases
--------------------------------------------------------------------------------

addEventListenerWithOptions
  ∷ ∀ opts opts'
  . Union opts Event.EventListenerOptions_Row opts'
  ⇒ Nub opts' Event.EventListenerOptions_Row
  ⇒ EventType
  → Record opts
  → (Event -> EditorM Unit)
  → EventTarget
  → EditorM Event.EventListenerInfo
addEventListenerWithOptions eventType options callback target = do
  ctx <- ask
  lift $ target # Event.addEventListenerWithOptions eventType options \event ->
    flip runReaderT ctx $ callback event

