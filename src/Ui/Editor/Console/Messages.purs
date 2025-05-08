module Ui.Editor.Console.Messages where

import Prelude

import Effect (Effect)
import Halogen.HTML (PlainHTML)

foreign import get_messages :: Effect (Array { timestamp :: Number, content :: PlainHTML })

foreign import get_timestamp :: Effect Number

foreign import push_message :: PlainHTML -> Effect Unit

