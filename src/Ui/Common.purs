module Ui.Common where

import Prelude

import Data.String as String
import Effect (Effect)
import Web.DOM (Document)
import Web.DOM as DOM
import Web.HTML (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument

--------------------------------------------------------------------------------
-- DOM
--------------------------------------------------------------------------------

foreign import htmlDoc :: HTMLDocument

doc :: Document
doc = htmlDoc # HTMLDocument.toDocument

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
