module Data.Expr where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

--------------------------------------------------------------------------------

data Expr = Expr Label (Array (Array Expr))

derive instance Generic Expr _

instance Show Expr where
  show x = genericShow x

--------------------------------------------------------------------------------

data Label = String String

derive instance Generic Label _

instance Show Label where
  show x = genericShow x

