module Ui.App1.Common where

import Prelude

import Data.Const (Const)
import Data.Eq.Generic (genericEq)
import Data.Expr (Expr, Fragment, Handle, Point)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Editor (Editor)
import Editor.Example.Editor2 (L)
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

type EditorInput =
  { editor :: Editor L
  }

type EditorOutput = Void

type EditorState =
  { editor :: Editor L
  , root :: Expr L
  , initial_mb_handle :: Maybe Handle
  , ref_mb_handle :: Ref (Maybe Handle)
  , ref_mb_dragOrigin :: Ref (Maybe Handle)
  , clipboard :: Maybe (Fragment L)
  }

data EditorAction
  = Initialize_EditorAction
  | PointOutput_EditorAction PointOutput
  | MouseUp_EditorAction Event
  | KeyDown_EditorAction Event
  | Rerender_EditorAction

type EditorSlots :: Row Type
type EditorSlots =
  ( "Point" :: H.Slot PointQuery PointOutput Point
  )

type EditorM = H.HalogenM EditorState EditorAction EditorSlots EditorOutput Aff

type EditorHTML = H.ComponentHTML EditorAction EditorSlots Aff

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

data PointQuery a =
  ModifyMaybeStatuses_PointQuery (Set PointStatus -> Set PointStatus) a

type PointInput =
  { point :: Point
  }

data PointOutput
  = MouseDown_PointOutput MouseEvent Point
  | MouseEnter_PointOutput MouseEvent Point

type PointState =
  { point :: Point
  , statuses :: Set PointStatus
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

data PointAction
  = Initialize_PointAction
  | Receive_PointAction PointInput
  | MouseDown_PointAction MouseEvent
  | MouseEnter_PointAction MouseEvent

type PointSlots :: Row Type
type PointSlots = ()

type PointM = H.HalogenM PointState PointAction PointSlots PointOutput Aff

type PointHTML = H.ComponentHTML PointAction PointSlots Aff

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

