module Data.Id where

import Effect (Effect)

foreign import fresh :: Effect String

