module Data.Trident where

import Prelude

data Trident a b c
  = First a
  | Second b
  | Third c
