module Ui.App1.Editor where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Ui.App1.Common (EditorAction, EditorInput, EditorOutput, EditorQuery, EditorSlots, EditorState, EditorHTML)
import Ui.Halogen (classes)

component :: H.Component EditorQuery EditorInput EditorOutput Aff
component = H.mkComponent { initialState, eval, render }

initialState :: EditorInput -> EditorState
initialState _input = {}

eval :: forall a. H.HalogenQ EditorQuery EditorAction EditorInput a -> H.HalogenM EditorState EditorAction EditorSlots EditorOutput Aff a
eval = H.mkEval H.defaultEval

render :: EditorState -> EditorHTML
render _state =
  HH.div [ classes [ "Editor" ] ]
    [ HH.text "{{Editor}}" ]

