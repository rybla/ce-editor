module Common where

import Prelude

import Data.String as String
import Halogen.HTML (PlainHTML)

type ConsoleMessage = { labels :: Array String, content :: PlainHTML }

showConsoleMessageLabels :: Array String -> String
showConsoleMessageLabels = String.joinWith " / "
