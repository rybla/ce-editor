module Ui.Editor.Common where

import Prelude

import Data.Const (Const)
import Data.Eq.Generic (genericEq)
import Data.Expr (Edit, EditMenu, Expr, Fragment, Handle, Point, PureEditorState)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Editor (Editor, Label)
import Editor.Common (ExistsEditor)
import Effect (Effect)
import Effect.Aff (Aff)
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
  , root :: Expr (Label c ())
  , initial_mb_handle :: Maybe Handle
  , ref_mb_handle :: Ref (Maybe Handle)
  , ref_mb_dragOrigin :: Ref (Maybe Handle)
  , clipboard :: Maybe (Fragment (Label c ()))
  , ref_history :: Ref (List (Snapshot c))
  , ref_future :: Ref (List (Snapshot c))
  }

toPureEditorState :: forall c. EditorState c -> Effect (PureEditorState (Label c ()))
toPureEditorState state = do
  mb_handle <- state.ref_mb_handle # Ref.read
  pure
    { root: state.root
    , mb_handle
    , clipboard: state.clipboard
    }

type Snapshot c =
  { root :: Expr (Label c ())
  , mb_handle :: Maybe Handle
  }

data EditorAction c
  = Initialize_EditorAction
  | Receive_EditorAction (EditorInput c)
  | PointOutput_EditorAction (PointOutput c)
  | MouseUp_EditorAction Event
  | KeyDown_EditorAction Event
  | Rerender_EditorAction

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
  , menu :: EditMenu (Label c ())
  }

data BufferOutput c =
  SubmitBuffer_BufferOutput (Edit (Label c ()))

type BufferState c =
  { editor :: Editor c
  , point :: Point
  , query :: String
  , menu :: EditMenu (Label c ())
  , option_i :: Maybe Int
  , menu_queried :: Array (String /\ Edit (Label c ()))
  }

data BufferAction
  = Initialize_BufferAction
  | KeyDown_BufferAction Event
  | QueryInput_BufferAction Event

type BufferSlots :: Row Type
type BufferSlots = ()

type BufferM c = H.HalogenM (BufferState c) BufferAction BufferSlots (BufferOutput c) Aff

type BufferHTML = H.ComponentHTML BufferAction BufferSlots Aff

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

