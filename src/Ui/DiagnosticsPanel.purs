module Ui.DiagnosticsPanel where

import Prelude

import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Ui.Editor.Common (DiagnosticsPanelAction(..), DiagnosticsPanelM)
import Ui.Editor.Config as Config
import Ui.Editor.Console.Messages as Console.Messages
import Ui.Halogen (classes)

component = H.mkComponent { initialState, eval, render }
  where
  initialState _input = {}

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  render _state =
    HH.div [ classes [ "DiagnosticsPanel" ] ]
      [ HH.text "<DiagnosticsPanel>" ]

handleAction ∷ DiagnosticsPanelAction → DiagnosticsPanelM Unit
handleAction Initialize_DiagnosticsPanelAction = do
  when Config.log_initializations do
    liftEffect $ Console.Messages.push_message $ HH.text $ "[DiagnosticsPanel.initialize]"

