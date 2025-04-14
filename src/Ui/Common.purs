-- | Importing this module means you **must** be in the browser environment
-- | (since the Javascript value `document` must be defined).
module Ui.Common where

import Prelude

import Data.String as String
import Effect (Effect)
import Prim.Row (class Nub, class Union)
import Record as Record
import Utility (todo)
import Web.DOM (Document, Element)
import Web.DOM as DOM
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (EventListener, EventTarget)
import Web.Event.EventTarget as EventTarget
import Web.HTML (HTMLBodyElement, HTMLDocument)
import Web.HTML.HTMLBodyElement as HTMLBodyElement
import Web.HTML.HTMLDocument as HTMLDocument

--------------------------------------------------------------------------------
-- DOM
--------------------------------------------------------------------------------

foreign import htmlDoc :: HTMLDocument

doc :: Document
doc = htmlDoc # HTMLDocument.toDocument

foreign import htmlBody :: HTMLBodyElement

body :: Element
body = htmlBody # HTMLBodyElement.toElement

--------------------------------------------------------------------------------
-- ConsoleMessage
--------------------------------------------------------------------------------

type ConsoleMessage = { labels :: Array String, content :: String }

showConsoleMessageLabels :: Array String -> String
showConsoleMessageLabels = String.joinWith " / "

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

foreign import setText_Element :: String -> DOM.Element -> Effect Unit

foreign import getText_Element :: DOM.Element -> Effect String

appendChild :: Element -> Element -> Effect Unit
appendChild child elem_parent = elem_parent # Element.toNode # Node.appendChild (child # Element.toNode)

createElement ∷ String → Element -> Effect Element
createElement tag parent = do
  elem <- doc # Document.createElement tag
  parent # appendChild elem
  pure elem

removeClass :: String -> Element -> Effect Unit
removeClass c elem = elem # Element.classList >>= (_ `DOMTokenList.remove` c)

addClass :: String -> Element -> Effect Unit
addClass c elem = elem # Element.classList >>= (_ `DOMTokenList.add` c)

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

foreign import shiftKey :: Event -> Boolean

