module Ui.DiagnosticsPanel where

import Prelude

import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (none)
import Editor (DiagnosticsPanelAction(..), Diagnostic(..))
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Ui.Editor.Common (DiagnosticsPanelQuery(..), DiagnosticsPanelM)
import Ui.Editor.Config as Config
import Ui.Editor.Console.Messages as Console.Messages
import Ui.Halogen (classes)

component = H.mkComponent { initialState, eval, render }
  where
  initialState _input =
    { diagnostics: none
    }

  eval = H.mkEval H.defaultEval
    { handleQuery = handleQuery
    , handleAction = handleAction
    }

  render state =
    HH.div [ classes [ "DiagnosticsPanel" ] ] $
      state.diagnostics # map \(Diagnostic d) ->
        HH.div [ classes [ "diagnostic" ] ]
          [ HH.div [ classes [ "title" ] ] [ HH.text d.title ]
          , HH.div [ classes [ "content" ] ] [ d.content ]
          ]

handleQuery :: forall a. DiagnosticsPanelQuery a -> DiagnosticsPanelM (Maybe a)
handleQuery (SetDiagnostics_DiagnosticsPanelQuery diagnostics a) = do
  modify_ _ { diagnostics = diagnostics }
  pure (Just a)

handleAction ∷ DiagnosticsPanelAction → DiagnosticsPanelM Unit
handleAction Initialize_DiagnosticsPanelAction = do
  when Config.log_initializations do
    liftEffect $ Console.Messages.push_message $ HH.text $ "[DiagnosticsPanel.initialize]"

