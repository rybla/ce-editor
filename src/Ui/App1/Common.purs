module Ui.App1.Common where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H

--------------------------------------------------------------------------------
-- App
--------------------------------------------------------------------------------

type AppQuery :: Type -> Type
type AppQuery = Const Void

type AppInput = {}

type AppOutput = Void

type AppState = {}

data AppAction

type AppSlots =
  ( "Editor" :: H.Slot EditorQuery EditorOutput Unit
  , "Console" :: H.Slot ConsoleQuery ConsoleOutput Unit
  )

type AppM = H.HalogenM AppState AppAction AppSlots AppOutput Aff

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

type EditorQuery :: Type -> Type
type EditorQuery = Const Void

type EditorInput = {}

type EditorOutput = Void

type EditorState = {}

data EditorAction

type EditorSlots :: Row Type
type EditorSlots = ()

type EditorM = H.HalogenM EditorState EditorAction EditorSlots EditorOutput Aff

--------------------------------------------------------------------------------
-- Console
--------------------------------------------------------------------------------

type ConsoleQuery :: Type -> Type
type ConsoleQuery = Const Void

type ConsoleInput = {}

type ConsoleOutput = Void

type ConsoleState = {}

data ConsoleAction

type ConsoleSlots :: Row Type
type ConsoleSlots = ()

type ConsoleM = H.HalogenM ConsoleState ConsoleAction ConsoleSlots ConsoleOutput Aff

