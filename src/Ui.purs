module Ui where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Ui.App as App
import Ui.Example.List as Example.List
import Utility (throwException)

main :: Effect Unit
main = launchAff_ case "Example.List" of
  "App" -> App.main
  "Example.List" -> Example.List.main
  label -> throwException $ "unrecognized main label: " <> show label

