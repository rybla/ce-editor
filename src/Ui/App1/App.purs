module Ui.App1.App where

import Prelude

-- import Editor.Example.Editor2 as ExampleEditor
-- import Editor.Example.Editor3 as ExampleEditor
-- import Editor.Example.Lisp as ExampleEditor
-- import Editor.Example.LispPlus as ExampleEditor
-- import Editor.Example.ULC as ExampleEditor
import Editor.Example.UlcV1 as ExampleEditor

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Ui.App1.Common (AppAction, AppInput, AppOutput, AppQuery, AppSlots, AppState, AppHTML)
-- import Ui.App1.Console as Console
import Ui.App1.Editor as Editor
import Ui.Halogen (classes)

component :: H.Component AppQuery AppInput AppOutput Aff
component = H.mkComponent { initialState, eval, render }

initialState :: AppInput -> AppState
initialState _input = {}

eval :: forall a. H.HalogenQ AppQuery AppAction AppInput a -> H.HalogenM AppState AppAction AppSlots AppOutput Aff a
eval = H.mkEval H.defaultEval

render :: AppState -> AppHTML
render _state =
  HH.div [ classes [ "App" ] ]
    [ HH.slot_ (Proxy @"Editor") unit Editor.component
        { editor: ExampleEditor.editor
        }
    -- , HH.slot_ (Proxy @"Console") unit Console.component {}
    ]

