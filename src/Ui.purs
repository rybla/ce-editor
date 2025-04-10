module Ui where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Ui.App as App
import Ui.Example.List as Example.List

main :: Effect Unit
main = case "App" of
  "App" -> App.main
  "Example.List" -> Example.List.main
  label -> liftEffect $ throw $ "unrecognized main label: " <> show label

