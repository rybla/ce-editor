module Ui.DiagnosticsPanel.Common where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H

--------------------------------------------------------------------------------
-- DiagnosticsPanel
--------------------------------------------------------------------------------

type DiagnosticsPanelQuery :: Type -> Type
type DiagnosticsPanelQuery = Const Void

type DiagnosticsPanelInput = {}

type DiagnosticsPanelOutput = Void

type DiagnosticsPanelState = {}

data DiagnosticsPanelAction = Initialize_DiagnosticsPanelAction

type DiagnosticsPanelSlots :: Row Type
type DiagnosticsPanelSlots = ()

type DiagnosticsPanelM = H.HalogenM DiagnosticsPanelState DiagnosticsPanelAction DiagnosticsPanelSlots DiagnosticsPanelOutput Aff

type DiagnosticsPanelHTML = H.ComponentHTML DiagnosticsPanelAction DiagnosticsPanelSlots Aff
