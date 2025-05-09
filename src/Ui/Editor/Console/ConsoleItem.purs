module Ui.Editor.Console.ConsoleItem where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ui.Element as Ui.Element
import Ui.Halogen (classes)
import Web.HTML.HTMLElement as HTMLElement

data Action =
  Initialize

component = H.mkComponent { initialState, eval, render }
  where
  refLabel_this = H.RefLabel "this"

  initialState input =
    { item: input.item
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = case _ of
        Initialize -> do
          H.getHTMLElementRef refLabel_this >>= case _ of
            Nothing -> pure unit
            Just element_this -> do
              element_this # HTMLElement.toElement # Ui.Element.scrollIntoView # liftEffect
    }

  render state =
    HH.div [ HP.ref refLabel_this, classes [ "item" ] ]
      [ HH.text $ show state.item.timestamp <> " "
      , state.item.content # fromPlainHTML
      ]

