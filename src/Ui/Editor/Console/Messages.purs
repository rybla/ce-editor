module Ui.Editor.Console.Messages where

import Prelude

import Effect (Effect)
import Halogen.HTML (PlainHTML)

foreign import get :: Effect (Array { timestamp :: Number, content :: PlainHTML })

foreign import push :: PlainHTML -> Effect Unit

