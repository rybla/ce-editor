module Ui.DiagnosticsPanel.Common where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Ui.Halogen (classes)

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

--------------------------------------------------------------------------------
-- Diagnostic
--------------------------------------------------------------------------------

data Diagnostic = Diagnostic DiagnosticComponent

mkDiagnostic
  :: { title :: String
     , body :: DiagnosticHTML
     }
  -> Diagnostic
mkDiagnostic args = Diagnostic $ H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}
  eval = H.mkEval H.defaultEval
  render _ =
    HH.div [ classes [ "Diagnostic" ] ]
      [ HH.div [ classes [ "title" ] ] [ HH.text args.title ]
      , HH.div [ classes [ "body" ] ] [ args.body ]
      ]

text :: String -> String -> Diagnostic
text title body = mkDiagnostic { title, body: HH.text body }

--------------------------------------------------------------------------------
-- DiagnosticComponent
--------------------------------------------------------------------------------

type DiagnosticComponent = H.Component DiagnosticQuery DiagnosticInput DiagnosticOutput Aff

type DiagnosticQuery :: Type -> Type
type DiagnosticQuery = Const Void

type DiagnosticInput = {}

type DiagnosticOutput = Void

type DiagnosticState = {}

data DiagnosticAction = Initialize_DiagnosticAction

type DiagnosticSlots :: Row Type
type DiagnosticSlots = ()

type DiagnosticM = H.HalogenM DiagnosticState DiagnosticAction DiagnosticSlots DiagnosticOutput Aff

type DiagnosticHTML = H.ComponentHTML DiagnosticAction DiagnosticSlots Aff

