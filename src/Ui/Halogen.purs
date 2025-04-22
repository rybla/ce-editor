module Ui.Halogen where

import Prelude

import Halogen as H
import Halogen.HTML.Properties as HP

classes ∷ ∀ w i. Array String → HP.IProp (class ∷ String | w) i
classes = HP.classes <<< map H.ClassName

