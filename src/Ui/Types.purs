module Ui.Types where

import Data.Expr
import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (get, put)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Editor (Editor)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Ui.Common (KeyInfo)
import Ui.Console as Console
import Utility (todo)
import Web.UIEvent.MouseEvent (MouseEvent)

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

data EditorQuery a = OtherEditorQuery a

type EditorInput = Editor

type EditorState =
  { editor :: Editor
  , expr :: Expr
  , max_history_length :: Int
  }

data EditorAction
  = Initialize
  | ViewExprOutput_Action ViewExprOutput
  | EngineOutput_Action EngineOutput
  | EngineQuery_Action (forall a. a -> EngineQuery a)

type EditorSlots =
  ( "Engine" :: H.Slot EngineQuery EngineOutput Unit
  , "Expr" :: H.Slot ViewExprQuery ViewExprOutput Unit
  )

data EditorOutput = TellConsole (forall a. a -> Console.Query a)

type EditorM' = ExceptT PlainHTML EditorM
type EditorM = H.HalogenM EditorState EditorAction EditorSlots EditorOutput Aff
type EditorHtml = H.ComponentHTML EditorAction EditorSlots EditorM

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

data EngineQuery a
  = ExprInteraction_EngineQuery Path ExprInteraction a
  | ViewPointInteraction_EngineQuery Point ViewPointInteraction a
  | EndDrag_EngineQuery a
  | BufferOutput_EngineQuery BufferOutput' a

type EngineInput =
  { editor :: Editor
  , expr :: Expr
  , handle :: Handle
  }

type EngineState =
  { editor :: Editor
  , expr :: Expr
  , handle :: Handle
  , history :: List Snapshot
  , future :: List Snapshot
  , drag_origin_handle :: Maybe Handle -- when dragging, this is the handle where the drag originated from
  , clipboard :: Maybe Fragment
  , bufferEnabled :: Boolean
  }

type Snapshot = { handle :: Handle, expr :: Expr }

data EngineAction
  = Initialize_EngineAction
  | Receive_EngineAction EngineInput
  | Keyboard_EngineAction KeyInfo

type EngineSlots = () :: Row Type

data EngineOutput
  = Output_EngineOutput EditorOutput
  | ViewExprQuery_EngineOutput (forall a. a -> ViewExprQuery a)
  | SetExpr_EngineOutput Expr

type EngineHTML = H.ComponentHTML EngineAction EngineSlots EngineM
type EngineM' = ExceptT PlainHTML EngineM
type EngineM = H.HalogenM EngineState EngineAction EngineSlots EngineOutput Aff

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

data ViewExprQuery a = ViewExprQuery (NonEmptyArray SingleViewExprQuery) a

data SingleViewExprQuery = SingleViewExprQuery Path ViewExprQuery'

data ViewExprQuery'
  = Modify_ViewExprQuery (ViewExprState -> ViewExprState)
  | ViewPointQuery_ViewExprQuery Index (ViewPointQuery Unit)

mkViewPointQuery_SingleViewExprQuery ∷ Point → ViewPointQuery Unit → SingleViewExprQuery
mkViewPointQuery_SingleViewExprQuery (Point p) q = SingleViewExprQuery p.path $ ViewPointQuery_ViewExprQuery p.j q

type ViewExprInput =
  { expr :: Expr
  }

type ViewExprState =
  { expr :: Expr
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

data ViewExprOutput
  = ViewExprOutput (Path /\ ViewExprOutput')
  | BufferOutput_ViewExprOutput BufferOutput'

data ViewExprOutput'
  = Output_ViewExprOutput EditorOutput
  | ExprInteraction ExprInteraction
  | ViewPointInteraction_ViewExprOutput Index ViewPointInteraction

data ExprInteraction
  = Click_ViewExprAction MouseEvent
  | StartDrag_ViewExprAction MouseEvent
  | MidDrag_ViewExprAction MouseEvent

type ViewExprHTML = H.ComponentHTML ViewExprAction ViewExprSlots ViewExprM
type ViewExprM' = ExceptT PlainHTML ViewExprM
type ViewExprM = H.HalogenM ViewExprState ViewExprAction ViewExprSlots ViewExprOutput Aff

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

data ViewPointQuery a
  = SetViewPointStyle_ViewPointQuery ViewPointStyle a
  | SetBufferEnabled_ViewPointQuery Boolean a

type ViewPointInput =
  {}

type ViewPointState =
  { style :: ViewPointStyle
  , bufferEnabled :: Boolean
  }

data ViewPointStyle
  = Plain_ViewPointStyle
  | Point_ViewPointStyle
  | Span_Left_ViewPointStyle
  | Span_Right_ViewPointStyle
  | Zipper_OuterLeft_ViewPointStyle
  | Zipper_InnerLeft_ViewPointStyle
  | Zipper_InnerRight_ViewPointStyle
  | Zipper_OuterRight_ViewPointStyle
  | Zipper_Inline_InnerLeft_And_InnerRight_ViewPointStyle
  | Zipper_Inline_OuterLeft_And_InnerLeft_ViewPointStyle
  | Zipper_Inline_InnerRight_And_OuterRight_ViewPointStyle
  | Zipper_OuterLeft_And_InnerLeft_ViewPointStyle
  | Zipper_InnerRight_And_OuterRight_ViewPointStyle
  | Zipper_InnerLeft_And_InnerRight_ViewPointStyle

derive instance Generic ViewPointStyle _

instance Show ViewPointStyle where
  show x = genericShow x

instance Eq ViewPointStyle where
  eq x = genericEq x

data ViewPointAction
  = Initialize_ViewPointAction
  | Receive_ViewPointAction ViewPointInput
  | ViewPointInteraction_ViewPointAction ViewPointInteraction
  | BufferOutput_ViewPointAction BufferOutput

type ViewPointSlots = ("Buffer" :: H.Slot BufferQuery BufferOutput Unit) :: Row Type

data ViewPointOutput
  = Output_ViewPointOutput EditorOutput
  | ViewPointInteraction ViewPointInteraction
  | BufferOutput_ViewPointOutput BufferOutput'

data ViewPointInteraction
  = StartDrag_ViewPointInteraction MouseEvent
  | MidDrag_ViewPointInteraction MouseEvent

type ViewPointHTML = H.ComponentHTML ViewPointAction ViewPointSlots ViewPointM
type ViewPointM' = ExceptT PlainHTML ViewPointM
type ViewPointM = H.HalogenM ViewPointState ViewPointAction ViewPointSlots ViewPointOutput Aff

--------------------------------------------------------------------------------
-- Buffer
--------------------------------------------------------------------------------

data BufferQuery a = Submit_BufferQuery a

type BufferInput = {}

type BufferState = {}

data BufferOutput
  = EditorOutput_BufferOutput EditorOutput
  | BufferOutput BufferOutput'

data BufferOutput' = UpdateQuery_BufferOutput String

data BufferAction
  = Initialize_BufferAction
  | Receive_BufferAction BufferInput

type BufferSlots = () :: Row Type

type BufferHTML = H.ComponentHTML BufferAction BufferSlots BufferM
type BufferM' = ExceptT PlainHTML BufferM
type BufferM = H.HalogenM BufferState BufferAction BufferSlots BufferOutput Aff

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

mkEval_with_error
  ∷ ∀ state query action slots input output m a
  . { finalize ∷ Maybe action
    , handleAction ∷ action -> ExceptT PlainHTML (H.HalogenM state action slots output m) Unit
    , handleQuery ∷ ∀ a'. query a' -> ExceptT PlainHTML (H.HalogenM state action slots output m) a'
    , initialize ∷ Maybe action
    , receive ∷ input -> Maybe action
    , trace :: Array String -> PlainHTML -> H.HalogenM state action slots output m Unit
    }
  → H.HalogenQ query action input a
  → H.HalogenM state action slots output m a
mkEval_with_error eval = H.mkEval
  { initialize: eval.initialize
  , handleQuery: \q -> do
      state <- get
      q # eval.handleQuery # runExceptT >>= case _ of
        Left err -> do
          put state
          eval.trace [ "Error" ] err
          pure Nothing
        Right a -> do
          pure (Just a)
  , handleAction: \q -> do
      state <- get
      q # eval.handleAction # runExceptT >>= case _ of
        Left err -> do
          put state
          eval.trace [ "Error" ] err
        Right it -> pure it
  , receive: eval.receive
  , finalize: eval.finalize
  }

