module Common where

import Prelude

import Halogen.HTML (PlainHTML)

type ConsoleMessage = { label :: String, content :: PlainHTML }

