module Data.Expr where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Ord.Generic (genericCompare)
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

data Label
  = Root
  | String String

derive instance Generic Label _

instance Show Label where
  show x = genericShow x

instance Eq Label where
  eq x = genericEq x

--------------------------------------------------------------------------------

-- the List Int is the steps along Expr kis
-- the final Int is the final point index in the last Expr's kids
data Index = Index (List Int) Int

derive instance Generic Index _

instance Show Index where
  show x = genericShow x

instance Eq Index where
  eq x = genericEq x

instance Ord Index where
  compare x = genericCompare x

data Handle
  = Cursor Index Index
  | Select Index Index Index Index

