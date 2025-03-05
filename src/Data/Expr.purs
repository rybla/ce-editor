module Data.Expr where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

--------------------------------------------------------------------------------

data Expr = Expr Label (Array Expr)

infix 0 Expr as %

derive instance Generic Expr _

instance Show Expr where
  show x = genericShow x

instance Eq Expr where
  eq x = genericEq x

--------------------------------------------------------------------------------

data Label = String String

derive instance Generic Label _

instance Show Label where
  show x = genericShow x

instance Eq Label where
  eq x = genericEq x