module Ui.App1.Browser where

import Prelude

import Effect (Effect)

-- navigator.clipboard.writeText()

foreign import navigator_clibpoard_writeText :: String -> Effect Unit