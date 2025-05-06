module Ui.App where

import Prelude

import Control.Monad.State (modify_)
import Data.Foldable (fold, foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe')
import Data.Tuple.Nested ((/\))
import Editor (Editor(..), Editor_ExistsLabel, mkEditor_ExistsLabel, runEditor_ExistsLabel)
import Editor.Example.Sexp as Editor.Example.Sexp
import Editor.Example.UlcV0 as Editor.Example.UlcV0
import Editor.Example.UlcV1 as Editor.Example.UlcV1
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Ui.Editor.Common (AppAction(..), AppHTML, AppInput, AppOutput, AppQuery, AppSlots, AppState)
import Ui.Editor.Editor as Editor
import Ui.Halogen (classes)
import Utility (impossible)

component :: H.Component AppQuery AppInput AppOutput Aff
component = H.mkComponent { initialState, eval, render }

editorsMenu :: Map String Editor_ExistsLabel
editorsMenu =
  editors
    # map (\editor_el -> editor_el # runEditor_ExistsLabel \(Editor editor) -> editor.name /\ editor_el)
    # Map.fromFoldable
  where
  editors =
    [ mkEditor_ExistsLabel Editor.Example.Sexp.editor
    , mkEditor_ExistsLabel Editor.Example.UlcV0.editor
    , mkEditor_ExistsLabel Editor.Example.UlcV1.editor
    ]

defaultEditor = mkEditor_ExistsLabel Editor.Example.Sexp.editor

initialState :: AppInput -> AppState
initialState _input = { mb_editor: pure defaultEditor }

eval :: forall a. H.HalogenQ AppQuery AppAction AppInput a -> H.HalogenM AppState AppAction AppSlots AppOutput Aff a
eval = H.mkEval H.defaultEval
  { handleAction = case _ of
      Initialize_AppAction -> pure unit
      SetEditor_AppAction editor_el -> modify_ _ { mb_editor = pure editor_el }
      Pass_AppAction -> pure unit
  }

render :: AppState -> AppHTML
render state =
  HH.div [ classes [ "App" ] ] $ fold
    [ [ HH.select
          [ HP.value $ defaultEditor # runEditor_ExistsLabel \(Editor editor) -> editor.name
          , HE.onValueChange case _ of
              name -> SetEditor_AppAction $ editorsMenu # Map.lookup name # fromMaybe' (impossible $ "unknown editor name: " <> name)
          ] $ editorsMenu # Map.toUnfoldable # map \(name /\ _) ->
          HH.option [ HP.value name ] [ HH.text name ]
      ]
    , state.mb_editor # foldMap \editor_el -> editor_el # runEditor_ExistsLabel \editor ->
        [ HH.slot_ (Proxy @"Editor") unit Editor.component
            { editor
            }
        ]
    -- , HH.slot_ (Proxy @"Console") unit Console.component {}
    ]

