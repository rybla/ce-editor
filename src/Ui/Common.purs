-- | Importing this module means you **must** be in the browser environment
-- | (since the Javascript value `document` must be defined).
module Ui.Common where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (and)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String as String
import Prim.Row (class Nub, class Union)
import Record as Record
import Type.Prelude (Proxy(..))
import Ui.Event as Event
import Web.Event.Event (Event)

--------------------------------------------------------------------------------
-- DOM
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ConsoleMessage
--------------------------------------------------------------------------------

type ConsoleMessage = { labels :: Array String, content :: String }

showConsoleMessageLabels :: Array String -> String
showConsoleMessageLabels = String.joinWith " / "

