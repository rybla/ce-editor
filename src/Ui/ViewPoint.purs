module Ui.ViewPoint where

import Prelude
import Ui.Types

import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (fold)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Prelude (Proxy(..))
import Ui.Buffer (buffer_component)
import Ui.Common (classes, text)
import Ui.Common as Ui
import Ui.Console as Console

viewPoint_component :: H.Component ViewPointQuery ViewPointInput ViewPointOutput Aff
viewPoint_component = H.mkComponent { initialState, eval, render }
  where
  initialState = initialViewPointStyle

  eval = mkEval_with_error
    { initialize: Just Initialize_ViewPointAction
    , handleQuery: handleViewPointQuery
    , handleAction: handleViewPointAction
    , receive: Just <<< Receive_ViewPointAction
    , finalize: Nothing
    , trace: traceViewPointM
    }

  render state =
    HH.div
      [ classes $ fold
          [ [ "Point" ]
          , [ state.style # show ]
          ]
      , HE.onMouseDown (StartDrag_ViewPointInteraction >>> ViewPointInteraction_ViewPointAction)
      , HE.onMouseEnter (MidDrag_ViewPointInteraction >>> ViewPointInteraction_ViewPointAction)
      ] $ Array.fold
      [ if not state.bufferEnabled then []
        else
          [ HH.div
              [ Ui.classes [ "BufferContainer" ] ]
              [ HH.slot (Proxy @"Buffer") unit buffer_component {} BufferOutput_ViewPointAction ]
          ]
      , [ text " " ]
      ]

initialViewPointStyle :: ViewPointInput -> ViewPointState
initialViewPointStyle _input =
  { style: Plain_ViewPointStyle
  , bufferEnabled: false
  }

handleViewPointQuery :: forall a. ViewPointQuery a -> ViewPointM' a
handleViewPointQuery (SetViewPointStyle_ViewPointQuery style a) = do
  modify_ _ { style = style, bufferEnabled = false }
  pure a
handleViewPointQuery (SetBufferEnabled_ViewPointQuery bufferEnabled a) = do
  modify_ _ { bufferEnabled = bufferEnabled }
  pure a

handleViewPointAction :: ViewPointAction -> ViewPointM' Unit
handleViewPointAction Initialize_ViewPointAction = do
  pure unit
handleViewPointAction (Receive_ViewPointAction input) = do
  state <- get
  let state' = initialViewPointStyle input
  when (state' /= state) do
    put state'
handleViewPointAction (ViewPointInteraction_ViewPointAction pi) = do
  lift $ H.raise $ ViewPointInteraction pi
handleViewPointAction (BufferOutput_ViewPointAction o) = do
  case o of
    EditorOutput_BufferOutput o -> lift $ H.raise $ Output_ViewPointOutput o
    BufferOutput o -> lift $ H.raise $ BufferOutput_ViewPointOutput o

--------------------------------------------------------------------------------

traceViewPointM :: Array String -> PlainHTML -> ViewPointM Unit
traceViewPointM labels content = H.raise $ Output_ViewPointOutput $ TellConsole \a -> Console.AddMessage { labels: [ "ViewPoint" ] <> labels, content } a
