module Ui.ViewPoint where

import Prelude
import Ui.Types

import Control.Monad.Except (runExceptT)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ui.Common (classes, text)
import Ui.Console as Console

viewPoint_component :: H.Component ViewPointQuery ViewPointInput ViewPointOutput Aff
viewPoint_component = H.mkComponent { initialState, eval, render }
  where
  initialState = initialViewPointStyle

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize_ViewPointAction
    , receive = pure <<< Receive_ViewPointAction
    , handleQuery = \query -> do
        state <- get
        handleViewPointQuery query # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewPointM [ "Error" ] err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleViewPointAction action # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewPointM [ "Error" ] err
          Right it -> pure it
    }

  render state =
    HH.div
      [ classes $ fold
          [ [ "Point" ]
          , [ state.style # show ]
          ]
      , HE.onMouseDown (StartDrag_ViewPointInteraction >>> ViewPointInteraction_ViewPointAction)
      , HE.onMouseEnter (MidDrag_ViewPointInteraction >>> ViewPointInteraction_ViewPointAction)
      ]
      [ if state.bufferEnabled then
          text "bufferEnabled"
        else
          text " "
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
  H.raise (ViewPointInteraction pi) # lift

--------------------------------------------------------------------------------

traceViewPointM :: Array String -> PlainHTML -> ViewPointM Unit
traceViewPointM labels content = H.raise $ Output_ViewPointOutput $ TellConsole \a -> Console.AddMessage { labels: [ "ViewPoint" ] <> labels, content } a
