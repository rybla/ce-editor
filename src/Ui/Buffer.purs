module Ui.Buffer where

import Prelude
import Ui.Common
import Ui.Types

import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ui.Common as Ui
import Ui.Console as Console
import Utility (todo)

buffer_component ∷ H.Component BufferQuery BufferInput BufferOutput Aff
buffer_component = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = mkEval_with_error
    { initialize: Just Initialize_BufferAction
    , handleQuery: handleBufferQuery
    , handleAction: handleBufferAction
    , receive: Just <<< Receive_BufferAction
    , finalize: Nothing
    , trace: traceBufferM
    }

  render state =
    HH.div
      [ Ui.classes [ "Buffer" ] ]
      [ HH.input [ HP.type_ InputText ] ]

initialBufferStyle :: BufferInput -> BufferState
initialBufferStyle {} = {}

handleBufferQuery :: forall a. BufferQuery a -> BufferM' a
handleBufferQuery (Submit_BufferQuery a) = pure a

handleBufferAction :: BufferAction -> BufferM' Unit
handleBufferAction Initialize_BufferAction = do
  lift $ traceBufferM [ "Initialize" ] $ text "initialize"
handleBufferAction (Receive_BufferAction input) = do
  state <- get
  let state' = initialBufferStyle input
  when (state' /= state) do
    put state'

--------------------------------------------------------------------------------

traceBufferM :: Array String -> PlainHTML -> BufferM Unit
traceBufferM labels content = H.raise $ EditorOutput_BufferOutput $ TellConsole \a -> Console.AddMessage { labels: [ "Buffer" ] <> labels, content } a

