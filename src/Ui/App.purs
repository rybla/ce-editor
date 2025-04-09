module Ui.App where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Either (either)
import Editor (Editor)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Prelude (Proxy(..))
import Ui.Common (code, span, style, text)
import Ui.Console (Query(..), Output, component) as Console
import Ui.Editor as Editor
import Ui.Types as Types
import Utility (format)

--------------------------------------------------------------------------------

type AppM' = ExceptT PlainHTML AppM

type AppM = H.HalogenM AppState AppAction AppSlots AppOutput Aff

type AppState =
  { editor :: Editor
  }

data AppAction
  = Initialize
  | ClickMe
  | EditorOutput Types.EditorOutput
  | OtherAction

type AppSlots =
  ( "Editor" :: H.Slot Types.EditorQuery Types.EditorOutput Unit
  , "Console" :: H.Slot Console.Query Console.Output Unit
  )

type AppOutput = Void

--------------------------------------------------------------------------------

component ∷ ∀ query. H.Component query Editor AppOutput Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState editor = { editor }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = \action -> do
        state <- get
        handleAction action # runExceptT >>= flip either pure \err -> do
          put state
          trace [ "App", "Error" ] err
        pure unit
    }

  render state =
    HH.div
      [ style do
          tell [ "height: 100vh", "width: 100vw" ]
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ style do
              tell [ "flex-grow: 0", "flex-shrink: 0" ]
              tell [ "background-color: black", "color: white" ]
              tell [ "display: flex", "flex-direction: row", "justify-content: space-between", "align-items: center" ]
          ]
          [ HH.div
              [ style do
                  tell [ "padding: 0.5em" ]
              ]
              [ text $ "ce-editor | {{name}}" # format { name: state.editor.name } ]
          , HH.div
              [ style do
                  tell [ "padding: 0.5em" ]
                  tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ]
              ]
              [ HH.div [] [ HH.button [ HE.onClick $ const ClickMe ] [ text "click me!" ] ]
              ]
          ]
      , HH.slot (Proxy @"Editor") unit Editor.editor_component state.editor EditorOutput
      , HH.slot_ (Proxy @"Console") unit Console.component unit
      ]

--------------------------------------------------------------------------------

handleAction :: AppAction -> AppM' Unit
handleAction Initialize = do
  lift $ trace [ "App", "Initialize" ] $ text "initialize"
  pure unit
handleAction (EditorOutput (Types.TellConsole q)) =
  H.tell (Proxy @"Console") unit q # lift
handleAction ClickMe = do
  lift $ trace [ "App", "ClickMe" ] $ HH.div []
    [ HH.div [] [ text "you clicked the button!" ]
    , HH.div [] [ text "you clicked the button!" ]
    , HH.div [] [ text "you clicked the button!" ]
    ]
  pure unit
handleAction _ =
  throwError $ span [ code "handleAction", text ": unimplemented action" ]

--------------------------------------------------------------------------------

trace :: Array String -> PlainHTML -> AppM Unit
trace labels content = H.tell (Proxy @"Console") unit $ Console.AddMessage { labels, content }

