module Ui.Event where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (all, and)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
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

shiftKey_Event :: Event -> Boolean
shiftKey_Event = lookup_unsafe "shiftKey"

ctrlKey_Event :: Event -> Boolean
ctrlKey_Event = lookup_unsafe "ctrlKey"

metaKey_Event :: Event -> Boolean
metaKey_Event = lookup_unsafe "metaKey"

altKey_Event :: Event -> Boolean
altKey_Event = lookup_unsafe "altKey"

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
        "ArrowUp" -> "↑"
        "ArrowDown" -> "↓"
        "ArrowRight" -> "→"
        "ArrowLeft" -> "←"
        "Backspace" -> "⌫"
        "Enter" -> "⏎"
        k -> k
    ]

fromEventToKeyInfo :: Event -> KeyInfo
fromEventToKeyInfo e = KeyInfo
  { key: key e
  , cmd: ctrlKey_Event e || metaKey_Event e
  , alt: altKey_Event e
  , shift: shiftKey_Event e
  }

mkKeyInfo
  ∷ ∀ (r ∷ Row Type) (r' ∷ Row Type)
  . Union r (key :: String, alt :: Boolean, cmd :: Boolean, shift :: Boolean) r'
  ⇒ Nub r' (key :: String, alt :: Boolean, cmd :: Boolean, shift :: Boolean)
  ⇒ String
  → Record r
  → KeyInfo
mkKeyInfo k r = KeyInfo (Record.merge r { key: k, cmd: false, shift: false, alt: false })

newtype KeyInfoPattern = KeyInfoPattern (Array KeyInfoPatternItem)

data KeyInfoPatternItem
  = KeyEq String
  | KeyMember (Set String)
  | KeyRegex Regex
  | CmdEq Boolean
  | AltEq Boolean
  | ShiftEq Boolean

keyEq = KeyEq
keyMember = KeyMember
keyRegex = KeyRegex
cmd = CmdEq true
not_cmd = CmdEq false
alt = AltEq true
not_alt = AltEq false
shift = ShiftEq true
not_shift = ShiftEq false

matchKeyInfoPattern :: KeyInfoPattern -> KeyInfo -> Boolean
matchKeyInfoPattern (KeyInfoPattern kpi) (KeyInfo ki) = and $ kpi # map case _ of
  KeyEq key_ -> ki.key == key_
  KeyMember keys -> Set.member ki.key keys
  KeyRegex regex -> Regex.test regex ki.key
  CmdEq cmd_ -> ki.cmd == cmd_
  AltEq alt_ -> ki.alt == alt_
  ShiftEq shift_ -> ki.shift == shift_

matchKeyInfoPattern' xs = matchKeyInfoPattern (KeyInfoPattern xs)

--------------------------------------------------------------------------------
-- old stuff for matchKeyInfo and such
--------------------------------------------------------------------------------

matchKeyInfo
  ∷ ∀ (r ∷ Row Type) (r' ∷ Row Type)
  . Union r (alt :: Maybe Boolean, cmd :: Maybe Boolean, shift :: Maybe Boolean) r'
  ⇒ Nub r' (alt :: Maybe Boolean, cmd :: Maybe Boolean, shift :: Maybe Boolean)
  ⇒ (KeyInfo → Boolean)
  → Record r
  → KeyInfo
  → Boolean
matchKeyInfo f r_ (KeyInfo ki) = and
  [ f (KeyInfo ki)
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
  ⇒ (KeyInfo → Maybe b)
  → Record r
  → KeyInfo
  → Maybe b
matchMapKeyInfo f r_ (KeyInfo ki) = do
  a <- f (KeyInfo ki)
  guard $ and
    [ r.cmd # maybe true (r'.cmd == _)
    , r.shift # maybe true (r'.shift == _)
    , r.alt # maybe true (r'.alt == _)
    ]
  pure a
  where
  r = Record.merge r_ { cmd: Nothing @Boolean, shift: Nothing @Boolean, alt: Nothing @Boolean }
  r' = Record.delete (Proxy @"key") ki

