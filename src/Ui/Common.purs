module Ui.Common where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Utility (todo)
import Web.DOM (Element)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------

span kids = HH.span [] kids
code str = HH.code [] [ HH.text str ]
text str = HH.text str

list ∷ ∀ (w ∷ Type) (i ∷ Type). Array (HTML w i) → HTML w i
list kids = HH.ul [] (kids # map \kid -> HH.li [] [ kid ])

column ∷ ∀ (w ∷ Type) (i ∷ Type). Array (HTML w i) → HTML w i
column kids = HH.div [] (kids # map \kid -> HH.div [] [ kid ])

classes ∷ ∀ (w ∷ Row Type) (i ∷ Type). Array String → IProp (class ∷ String | w) i
classes cns = HP.classes (cns # map HH.ClassName)

--------------------------------------------------------------------------------

style :: forall w i. Writer (Array String) Unit -> IProp (style :: String | w) i
style w = HP.style $ intercalate "; " $ execWriter w

--------------------------------------------------------------------------------

foreign import scrollIntoView :: Element -> Effect Unit

newtype KeyInfo = KeyInfo
  { key :: String
  , cmd :: Boolean
  , shift :: Boolean
  }

derive instance Newtype KeyInfo _

instance Show KeyInfo where
  show (KeyInfo ki) = Array.fold
    [ if ki.shift then "" else "^"
    , if ki.cmd then "" else "⌘"
    , case ki.key of
        -- TODO: special cases
        key -> key
    ]

fromKeyboardEventToKeyInfo :: KeyboardEvent -> KeyInfo
fromKeyboardEventToKeyInfo ke = KeyInfo
  { key: KeyboardEvent.key ke
  , cmd: KeyboardEvent.ctrlKey ke || KeyboardEvent.metaKey ke
  , shift: KeyboardEvent.shiftKey ke
  }
