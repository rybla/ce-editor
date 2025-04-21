module Ui.Event where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (and)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Prim.Row (class Nub, class Union)
import Record as Record
import Type.Prelude (Proxy(..))
import Utility (lookup_unsafe)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (EventListener, EventTarget)
import Web.Event.EventTarget as EventTarget

type EventListenerInfo =
  { eventType :: EventType
  , eventListener :: EventListener
  , options :: EventListenerOptions
  }

type EventListenerOptions = Record EventListenerOptions_Row
type EventListenerOptions_Row =
  ( -- Whether or not to dispatch event to this listener before dispatching to 
    -- listeners below this node in the DOM tree.
    capture :: Boolean
  -- Whether or not this listener can be invoked at most once.
  -- If true, then is automatically removed after first invokation).
  -- Default is `false`.
  , once :: Boolean
  -- Whether or not the listener cannot call `preventDefault`.
  -- Default is `false`.
  , passive :: Boolean
  )

removeEventListener :: EventListenerInfo -> EventTarget -> Effect Unit
removeEventListener { eventType, eventListener, options: { capture } } target = do
  target # EventTarget.removeEventListener eventType eventListener capture

addEventListenerWithOptions
  ∷ ∀ opts opts'
  . Union opts EventListenerOptions_Row opts'
  ⇒ Nub opts' EventListenerOptions_Row
  ⇒ EventType
  → Record opts
  → (Event -> Effect Unit)
  → EventTarget
  → Effect EventListenerInfo
addEventListenerWithOptions eventType options callback target = do
  let options' = options `Record.merge` { capture: false, once: false, passive: false }
  eventListener <- EventTarget.eventListener callback
  target # EventTarget.addEventListenerWithOptions eventType eventListener options'
  pure { eventType, eventListener, options: options' }

key :: Event -> String
key = lookup_unsafe "key"

shiftKey :: Event -> Boolean
shiftKey = lookup_unsafe "shiftKey"

ctrlKey :: Event -> Boolean
ctrlKey = lookup_unsafe "ctrlKey"

metaKey :: Event -> Boolean
metaKey = lookup_unsafe "metaKey"

altKey :: Event -> Boolean
altKey = lookup_unsafe "altKey"

newtype KeyInfo = KeyInfo
  { key :: String
  , cmd :: Boolean
  , alt :: Boolean
  , shift :: Boolean
  }

derive instance Newtype KeyInfo _

derive newtype instance Eq KeyInfo

instance Show KeyInfo where
  show (KeyInfo ki) = Array.fold
    [ if ki.shift then "^" else ""
    , if ki.cmd then "⌘" else ""
    , if ki.alt then "⎇" else ""
    , case ki.key of
        -- TODO: special cases
        k -> k
    ]

fromEventToKeyInfo :: Event -> KeyInfo
fromEventToKeyInfo e = KeyInfo
  { key: key e
  , cmd: ctrlKey e || metaKey e
  , alt: altKey e
  , shift: shiftKey e
  }

mkKeyInfo
  ∷ ∀ (r ∷ Row Type) (r' ∷ Row Type)
  . Union r (key :: String, alt :: Boolean, cmd :: Boolean, shift :: Boolean) r'
  ⇒ Nub r' (key :: String, alt :: Boolean, cmd :: Boolean, shift :: Boolean)
  ⇒ String
  → Record r
  → KeyInfo
mkKeyInfo k r = KeyInfo (Record.merge r { key: k, cmd: false, shift: false, alt: false })

matchKeyInfo
  ∷ ∀ (r ∷ Row Type) (r' ∷ Row Type)
  . Union r (alt :: Maybe Boolean, cmd :: Maybe Boolean, shift :: Maybe Boolean) r'
  ⇒ Nub r' (alt :: Maybe Boolean, cmd :: Maybe Boolean, shift :: Maybe Boolean)
  ⇒ (String → Boolean)
  → Record r
  → KeyInfo
  → Boolean
matchKeyInfo f r_ (KeyInfo ki) = and
  [ f ki.key
  , r.cmd # maybe true (r'.cmd == _)
  , r.shift # maybe true (r'.shift == _)
  ]
  where
  r = Record.merge r_ { cmd: Nothing @Boolean, shift: Nothing @Boolean, alt: Nothing @Boolean }
  r' = Record.delete (Proxy @"key") ki

matchMapKeyInfo
  ∷ ∀ (r ∷ Row Type) (r' ∷ Row Type) (b ∷ Type)
  . Union r (alt :: Maybe Boolean, cmd :: Maybe Boolean, shift :: Maybe Boolean) r'
  ⇒ Nub r' (alt :: Maybe Boolean, cmd :: Maybe Boolean, shift :: Maybe Boolean)
  ⇒ (String → Maybe b)
  → Record r
  → KeyInfo
  → Maybe b
matchMapKeyInfo f r_ (KeyInfo ki) = do
  a <- f ki.key
  guard $ and
    [ r.cmd # maybe true (r'.cmd == _)
    , r.shift # maybe true (r'.shift == _)
    , r.alt # maybe true (r'.alt == _)
    ]
  pure a
  where
  r = Record.merge r_ { cmd: Nothing @Boolean, shift: Nothing @Boolean, alt: Nothing @Boolean }
  r' = Record.delete (Proxy @"key") ki

