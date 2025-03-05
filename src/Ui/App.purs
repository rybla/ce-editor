module Ui.App where

import Prelude

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
import Ui.Common (style)
import Ui.Console (Query(..), component) as Console
import Utility (bug, format)

type EHM = ExceptT PlainHTML HM

type HM = H.HalogenM State Action Slots Output M

type M = Aff

type State =
  { editor :: Editor
  }

data Action
  = Initialize
  | ClickMe
  | OtherActions

type Slots =
  ( "Console" :: H.Slot Console.Query Void Unit
  )

type Output = Void

component ∷ ∀ query. H.Component query Editor Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState editor = { editor }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = \action -> do
        state <- get
        handleAction action # runExceptT >>= flip either pure \err -> do
          put state
          trace "error" err
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
              [ HH.text $ "ce-editor | {{name}}" # format { name: state.editor.name } ]
          , HH.div
              [ style do
                  tell [ "padding: 0.5em" ]
                  tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ]
              ]
              [ HH.div [] [ HH.button [ HE.onClick $ const ClickMe ] [ HH.text "click me!" ] ]
              , HH.div [] [ HH.button [ HE.onClick $ const ClickMe ] [ HH.text "click me!" ] ]
              , HH.div [] [ HH.button [ HE.onClick $ const ClickMe ] [ HH.text "click me!" ] ]
              , HH.div [] [ HH.button [ HE.onClick $ const ClickMe ] [ HH.text "click me!" ] ]
              ]
          ]
      , HH.div
          [ style do
              tell [ "flex-grow: 1", "flex-shrink: 1" ]
              tell [ "padding: 0.5em" ]
          ]
          [ HH.text "{{Editor}}" ]
      , HH.slot_ (Proxy @"Console") unit Console.component unit
      ]

--------------------------------------------------------------------------------

handleAction :: Action -> EHM Unit
handleAction Initialize = do
  lift $ trace "App" $ HH.text "initialized"
  pure unit
handleAction ClickMe = do
  lift $ trace "App" $ HH.div []
    [ HH.div [] [ HH.text "you clicked the button!" ]
    , HH.div [] [ HH.text "you clicked the button!" ]
    , HH.div [] [ HH.text "you clicked the button!" ]
    ]
  pure unit
handleAction _ = bug "unimplemented action"

--------------------------------------------------------------------------------

trace :: String -> PlainHTML -> HM Unit
trace label content = H.tell (Proxy @"Console") unit $ Console.AddMessage { label, content }

code str = HH.code [] [ HH.text str ]
text str = HH.text str

