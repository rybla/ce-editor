module Ui where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Ui.App as App

main :: Effect Unit
main = launchAff_ App.main

