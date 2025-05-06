module Ui.Editor.Common where

import Prelude

import Control.Monad.State (get)
import Data.Const (Const)
import Data.Eq.Generic (genericEq)
import Data.Expr (Edit, EditMenu, Expr, Fragment, Handle, Point, BasicEditorState)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Editor (Editor, Label, StampedLabel)
import Editor.Common (ExistsEditor)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent)

--------------------------------------------------------------------------------
-- App
--------------------------------------------------------------------------------

type AppQuery :: Type -> Type
type AppQuery = Const Void

type AppInput = {}

type AppOutput = Void

type AppState =
  { mb_editor :: Maybe ExistsEditor
  }

data AppAction
  = Initialize_AppAction
  | SetEditor_AppAction ExistsEditor
  | Pass_AppAction

type AppSlots =
  ( "Editor" :: H.Slot EditorQuery EditorOutput Unit
  , "Console" :: H.Slot ConsoleQuery ConsoleOutput Unit
  )

type AppM = H.HalogenM AppState AppAction AppSlots AppOutput Aff

type AppHTML = H.ComponentHTML AppAction AppSlots Aff

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

type EditorQuery :: Type -> Type
type EditorQuery = Const Void

type EditorInput c =
  { editor :: Editor c
  }

type EditorOutput = Void

type EditorState c =
  { editor :: Editor c
  , mb_root :: Maybe (Expr (StampedLabel c ()))
  , initial_mb_handle :: Maybe Handle
  , ref_mb_handle :: Ref (Maybe Handle)
  , ref_mb_dragOrigin :: Ref (Maybe Handle)
  , clipboard :: Maybe (Fragment (StampedLabel c ()))
  , ref_history :: Ref (List (Snapshot c))
  , ref_future :: Ref (List (Snapshot c))
  }

getBasicEditorState :: forall c. EditorM c (BasicEditorState (StampedLabel c ()))
getBasicEditorState = get >>= toBasicEditorState >>> liftEffect

getRoot :: forall c. EditorM c (Expr (StampedLabel c ()))
getRoot = get >>= \state -> case state.mb_root of
  Nothing -> liftEffect $ throw "root not loaded yet"
  Just root -> pure root

toBasicEditorState :: forall c. EditorState c -> Effect (BasicEditorState (StampedLabel c ()))
toBasicEditorState state = do
  root <- case state.mb_root of
    Nothing -> throw "root not loaded yet"
    Just root -> pure root
  mb_handle <- state.ref_mb_handle # Ref.read
  pure
    { root
    , mb_handle
    , clipboard: state.clipboard
    }

type Snapshot c =
  { root :: Expr (StampedLabel c ())
  , mb_handle :: Maybe Handle
  }

data EditorAction c
  = Initialize_EditorAction
  | Receive_EditorAction (EditorInput c)
  | PointOutput_EditorAction (PointOutput c)
  | MouseUp_EditorAction Event
  | KeyDown_EditorAction Event

type EditorSlots c =
  ( "Point" :: H.Slot (PointQuery c) (PointOutput c) Point
  )

type EditorM c = H.HalogenM (EditorState c) (EditorAction c) (EditorSlots c) EditorOutput Aff

type EditorHTML c = H.ComponentHTML (EditorAction c) (EditorSlots c) Aff

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

data PointQuery c a
  = ModifyStatuses_PointQuery (Set PointStatus -> Set PointStatus) a
  | SetBufferInput_PointQuery (Maybe (BufferInput c)) a
  | GetBufferInput_PointQuery (Maybe (BufferInput c) -> a)

type PointInput c =
  { editor :: Editor c
  , point :: Point
  }

data PointOutput c
  = MouseDown_PointOutput MouseEvent Point
  | MouseEnter_PointOutput MouseEvent Point
  | BufferOutput_PointOutput (BufferOutput c)

type PointState c =
  { editor :: Editor c
  , point :: Point
  , statuses :: Set PointStatus
  , mb_bufferInput :: Maybe (BufferInput c)
  }

data PointStatus
  = Point_Handle_PointStatus
  | SpanH_Handle_Left_PointStatus
  | SpanH_Handle_Right_PointStatus
  | ZipperH_Handle_OuterLeft_PointStatus
  | ZipperH_Handle_InnerLeft_PointStatus
  | ZipperH_Handle_InnerRight_PointStatus
  | ZipperH_Handle_OuterRight_PointStatus
  | LeftFocus_PointStatus
  | RightFocus_PointStatus

derive instance Generic PointStatus _

instance Show PointStatus where
  show x = genericShow x

instance Eq PointStatus where
  eq x = genericEq x

instance Ord PointStatus where
  compare x = genericCompare x

data PointAction c
  = Initialize_PointAction
  | Receive_PointAction (PointInput c)
  | MouseDown_PointAction MouseEvent
  | MouseEnter_PointAction MouseEvent
  | BufferOutput_PointAction (BufferOutput c)

type PointSlots c = ("Buffer" :: H.Slot BufferQuery (BufferOutput c) Unit)

type PointM c = H.HalogenM (PointState c) (PointAction c) (PointSlots c) (PointOutput c) Aff

type PointHTML c = H.ComponentHTML (PointAction c) (PointSlots c) Aff

--------------------------------------------------------------------------------
-- Buffer
--------------------------------------------------------------------------------

type BufferQuery :: Type -> Type
type BufferQuery = Const Void

-- TODO: what stuff does the buffer need to know about? can't it just have a
-- list of edits that have already been computed and then the buffer is justt
-- searching through them, right?
type BufferInput c =
  { editor :: Editor c
  , point :: Point
  , query :: String
  , menu :: EditMenu Aff (Label c ()) (StampedLabel c ())
  }

data BufferOutput c = SubmitBuffer_BufferOutput (Edit Aff (Label c ()) (StampedLabel c ()))

type BufferState c =
  { editor :: Editor c
  , point :: Point
  , query :: String
  , menu :: EditMenu Aff (Label c ()) (StampedLabel c ())
  , option_i :: Maybe Int
  , menu_queried :: Array (String /\ Edit Aff (Label c ()) (StampedLabel c ()))
  }

data BufferAction c
  = Initialize_BufferAction
  | Receive_BufferAction (BufferInput c)
  | KeyDown_BufferAction Event
  | QueryInput_BufferAction Event

type BufferSlots :: Row Type
type BufferSlots = ()

type BufferM c = H.HalogenM (BufferState c) (BufferAction c) BufferSlots (BufferOutput c) Aff

type BufferHTML c = H.ComponentHTML (BufferAction c) BufferSlots Aff

--------------------------------------------------------------------------------
-- Console
--------------------------------------------------------------------------------

type ConsoleQuery :: Type -> Type
type ConsoleQuery = Const Void

type ConsoleInput = {}

type ConsoleOutput = Void

type ConsoleState = {}

data ConsoleAction = Initialize_ConsoleAction

type ConsoleSlots :: Row Type
type ConsoleSlots = ()

type ConsoleM = H.HalogenM ConsoleState ConsoleAction ConsoleSlots ConsoleOutput Aff

type ConsoleHTML = H.ComponentHTML ConsoleAction ConsoleSlots Aff

