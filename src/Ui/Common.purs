module Ui.Common where

import Prelude

import Data.String as String

type ConsoleMessage = { labels :: Array String, content :: String }

showConsoleMessageLabels :: Array String -> String
showConsoleMessageLabels = String.joinWith " / "

