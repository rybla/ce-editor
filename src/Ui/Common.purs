module Ui.Common where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Writer (Writer, execWriter)
import Data.Array as Array
import Data.Foldable (and, intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Record as Record
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

span kids = HH.span [] kids
code str = HH.code [] [ HH.text str ]
text str = HH.text str

error kids = HH.span [ HP.style "display: inline-block; background-color: color-mix(in rgb, red, transparent 50%);" ] kids

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

derive newtype instance Eq KeyInfo

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

mkKeyInfo key r = KeyInfo (Record.merge r { key, cmd: false, shift: false })

matchKeyInfo f r_ (KeyInfo ki) = and
  [ f ki.key
  , r.cmd # maybe true (r'.cmd == _)
  , r.shift # maybe true (r'.shift == _)
  ]
  where
  r = Record.merge r_ { cmd: Nothing @Boolean, shift: Nothing @Boolean }
  r' = Record.delete (Proxy @"key") ki

matchMapKeyInfo f r_ (KeyInfo ki) = do
  a <- f ki.key
  guard $ and
    [ r.cmd # maybe true (r'.cmd == _)
    , r.shift # maybe true (r'.shift == _)
    ]
  pure a
  where
  r = Record.merge r_ { cmd: Nothing @Boolean, shift: Nothing @Boolean }
  r' = Record.delete (Proxy @"key") ki

