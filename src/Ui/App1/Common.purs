module Ui.App1.Common where

import Prelude

import Data.Const (Const)
import Data.Eq.Generic (genericEq)
import Data.Expr (Edit, EditMenu, Expr, Fragment, Handle, Point)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Editor (Editor)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
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

type AppState = {}

data AppAction = Initialize_AppAction

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

type EditorInput l =
  { editor :: Editor l
  }

type EditorOutput = Void

type EditorState l =
  { editor :: Editor l
  , root :: Expr l
  , initial_mb_handle :: Maybe Handle
  , ref_mb_handle :: Ref (Maybe Handle)
  , ref_mb_dragOrigin :: Ref (Maybe Handle)
  , clipboard :: Maybe (Fragment l)
  , ref_history :: Ref (List (Snapshot l))
  , ref_future :: Ref (List (Snapshot l))
  }

type Snapshot l =
  { root :: Expr l
  , mb_handle :: Maybe Handle
  }

data EditorAction l
  = Initialize_EditorAction
  | PointOutput_EditorAction (PointOutput l)
  | MouseUp_EditorAction Event
  | KeyDown_EditorAction Event
  | Rerender_EditorAction

type EditorSlots l =
  ( "Point" :: H.Slot (PointQuery l) (PointOutput l) Point
  )

type EditorM l = H.HalogenM (EditorState l) (EditorAction l) (EditorSlots l) EditorOutput Aff

type EditorHTML l = H.ComponentHTML (EditorAction l) (EditorSlots l) Aff

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

data PointQuery l a
  = ModifyStatuses_PointQuery (Set PointStatus -> Set PointStatus) a
  | SetBufferInput_PointQuery (Maybe (BufferInput l)) a
  | GetBufferInput_PointQuery (Maybe (BufferInput l) -> a)

type PointInput l =
  { editor :: Editor l
  , point :: Point
  }

data PointOutput l
  = MouseDown_PointOutput MouseEvent Point
  | MouseEnter_PointOutput MouseEvent Point
  | BufferOutput_PointOutput (BufferOutput l)

type PointState l =
  { editor :: Editor l
  , point :: Point
  , statuses :: Set PointStatus
  , mb_bufferInput :: Maybe (BufferInput l)
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

data PointAction l
  = Initialize_PointAction
  | Receive_PointAction (PointInput l)
  | MouseDown_PointAction MouseEvent
  | MouseEnter_PointAction MouseEvent
  | BufferOutput_PointAction (BufferOutput l)

type PointSlots l = ("Buffer" :: H.Slot BufferQuery (BufferOutput l) Unit)

type PointM l = H.HalogenM (PointState l) (PointAction l) (PointSlots l) (PointOutput l) Aff

type PointHTML l = H.ComponentHTML (PointAction l) (PointSlots l) Aff

--------------------------------------------------------------------------------
-- Buffer
--------------------------------------------------------------------------------

type BufferQuery :: Type -> Type
type BufferQuery = Const Void

-- TODO: what stuff does the buffer need to know about? can't it just have a
-- list of edits that have already been computed and then the buffer is justt
-- searching through them, right?
type BufferInput l =
  { editor :: Editor l
  , point :: Point
  , query :: String
  , menu :: EditMenu l
  }

data BufferOutput l =
  SubmitBuffer_BufferOutput (Edit l)

type BufferState l =
  { editor :: Editor l
  , point :: Point
  , query :: String
  , menu :: EditMenu l
  , option_i :: Maybe Int
  , menu_queried :: Array (Edit l)
  }

data BufferAction
  = Initialize_BufferAction
  | KeyDown_BufferAction Event
  | QueryInput_BufferAction Event

type BufferSlots :: Row Type
type BufferSlots = ()

type BufferM l = H.HalogenM (BufferState l) BufferAction BufferSlots (BufferOutput l) Aff

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

