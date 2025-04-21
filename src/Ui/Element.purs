module Ui.Element where

import Prelude

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

foreign import htmlDoc :: HTMLDocument

doc :: Document
doc = htmlDoc # HTMLDocument.toDocument

foreign import htmlBody :: HTMLBodyElement

body :: Element
body = htmlBody # HTMLBodyElement.toElement

foreign import setText :: String -> DOM.Element -> Effect Unit

foreign import getText :: DOM.Element -> Effect String

appendChild :: Element -> Element -> Effect Unit
appendChild child parent = parent # Element.toNode # Node.appendChild (child # Element.toNode)

removeChild ∷ Element → Element → Effect Unit
removeChild child parent = parent # Element.toNode # Node.removeChild (child # Element.toNode)

foreign import getChildren :: Element -> Effect (Array Element)

foreign import removeAllChildren :: Element -> Effect Unit

-- | replaceChild old_child new_child parent
replaceChild ∷ Element → Element → Element → Effect Unit
replaceChild old_child new_child parent = parent # Element.toNode # Node.replaceChild (new_child # Element.toNode) (old_child # Element.toNode)

createChild ∷ String → Element -> Effect Element
createChild tag parent = do
  elem <- doc # Document.createElement tag
  parent # appendChild elem
  pure elem

create ∷ String → Effect Element
create tag = doc # Document.createElement tag

removeClass :: String -> Element -> Effect Unit
removeClass c elem = elem # Element.classList >>= (_ `DOMTokenList.remove` c)

addClass :: String -> Element -> Effect Unit
addClass c elem = elem # Element.classList >>= (_ `DOMTokenList.add` c)

