module Ui.Buffer where

import Prelude
import Ui.Common
import Ui.Types

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ui.Common as Ui

buffer_component ∷ H.Component BufferQuery BufferInput BufferOutput Aff
buffer_component = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval
    { initialize = Just Initialize_BufferAction
    , receive = Just <<< Receive_BufferAction
    }

  render state =
    HH.div
      [ Ui.classes [ "Buffer" ] ]
      [ HH.input [ HP.type_ InputText ] ]

