module Ui.App1.Buffer where

import Prelude

import Data.Const (Const(..))
import Data.Expr (BufferOption(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Ui.App1.Common (BufferAction(..), BufferHTML, BufferInput, BufferM, BufferOutput, BufferQuery, BufferSlots, BufferState)
import Ui.Halogen (classes)

component :: H.Component BufferQuery BufferInput BufferOutput Aff
component = H.mkComponent { initialState, eval, render }

initialState :: BufferInput -> BufferState
initialState input =
  { query
  , options: input.options
  , options_queried: input.options query
  }
  where
  query = ""

eval :: forall a. H.HalogenQ BufferQuery BufferAction BufferInput a -> H.HalogenM BufferState BufferAction BufferSlots BufferOutput Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_BufferAction
  , handleQuery = handleQuery
  , handleAction = handleAction
  }

handleQuery :: forall a. BufferQuery a -> BufferM (Maybe a)
handleQuery (Const x) = absurd x

handleAction :: BufferAction -> BufferM Unit
handleAction Initialize_BufferAction = do
  Console.log "[Buffer] initialize"
  pure unit

render :: BufferState -> BufferHTML
render state =
  HH.div [ classes $ fold [ [ "Buffer" ] ] ]
    [ HH.input []
    , HH.div [] $ state.options_queried # map case _ of
        PasteSpan_BufferOption label span ->
          HH.div [ classes [ "PasteSpan", "BufferOption" ] ]
            [ HH.div [ classes [ "label" ] ] [ HH.text label ]
            , HH.div [ classes [ "body" ] ] [ HH.text $ show span ]
            ]
    ]

