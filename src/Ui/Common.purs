module Ui.Common where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Data.Foldable (intercalate)
import Effect (Effect)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Web.DOM (Element)

--------------------------------------------------------------------------------

span kids = HH.span [] kids
code str = HH.code [] [ HH.text str ]
text str = HH.text str

--------------------------------------------------------------------------------

style :: forall w i. Writer (Array String) Unit -> IProp (style :: String | w) i
style w = HP.style $ intercalate "; " $ execWriter w

--------------------------------------------------------------------------------

foreign import scrollIntoView :: Element -> Effect Unit
