module Pretty where

import Prelude

class Pretty a where
  pretty :: a -> String

parens s = "(" <> s <> ")"
