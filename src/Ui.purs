module Ui where

import Prelude

import Editor (editor)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Ui.App as App

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI App.component editor =<< HA.awaitBody)

