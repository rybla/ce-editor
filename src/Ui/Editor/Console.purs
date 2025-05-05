module Ui.Editor.Console where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Ui.Editor.Common (ConsoleAction, ConsoleInput, ConsoleOutput, ConsoleQuery, ConsoleSlots, ConsoleState, ConsoleHTML)
import Ui.Halogen (classes)

component :: H.Component ConsoleQuery ConsoleInput ConsoleOutput Aff
component = H.mkComponent { initialState, eval, render }

initialState :: ConsoleInput -> ConsoleState
initialState _input = {}

eval :: forall a. H.HalogenQ ConsoleQuery ConsoleAction ConsoleInput a -> H.HalogenM ConsoleState ConsoleAction ConsoleSlots ConsoleOutput Aff a
eval = H.mkEval H.defaultEval

render :: ConsoleState -> ConsoleHTML
render _state =
  HH.div [ classes [ "Console" ] ]
    [ HH.text "{{Console}}" ]

