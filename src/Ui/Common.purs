module Ui.Common where

import Prelude

import Data.String as String

--------------------------------------------------------------------------------
-- ConsoleMessage
--------------------------------------------------------------------------------

type ConsoleMessage = { labels :: Array String, content :: String }

showConsoleMessageLabels :: Array String -> String
showConsoleMessageLabels = String.joinWith " / "

