module Ui where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Ui.App as App
import Ui.Example.List as Example.List
import Utility (throwException)

main :: Effect Unit
main = launchAff_ case "" of
  "Example.List" -> Example.List.main
  "App" -> App.main
  label -> throwException $ "unrecognized main label: " <> show label

