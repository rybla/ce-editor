-- | Importing this module means you **must** be in the browser environment
-- | (since the Javascript value `document` must be defined).
module Ui.Common where

import Prelude

import Data.String as String
import Effect (Effect)
import Web.DOM (Document, Element)
import Web.DOM as DOM
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
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

