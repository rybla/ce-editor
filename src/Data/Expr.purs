module Data.Expr where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (todo)

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
data Point = Point (List Int) Int

derive instance Generic Point _

instance Show Point where
  show x = genericShow x

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare x = genericCompare x

data Handle
  = Cursor_Handle Cursor
  | Select_Handle Select

derive instance Generic Handle _

instance Show Handle where
  show x = genericShow x

instance Eq Handle where
  eq x = genericEq x

data Cursor = Cursor Point Point CursorFocus

derive instance Generic Cursor _

instance Show Cursor where
  show x = genericShow x

instance Eq Cursor where
  eq x = genericEq x

data CursorFocus = Left_CursorFocus | Right_CursorFocus

derive instance Generic CursorFocus _

instance Show CursorFocus where
  show x = genericShow x

instance Eq CursorFocus where
  eq x = genericEq x

data Select = Select Point Point Point Point SelectFocus

derive instance Generic Select _

instance Show Select where
  show x = genericShow x

instance Eq Select where
  eq x = genericEq x

data SelectFocus
  = OuterLeft_SelectFocus
  | InnerLeft_SelectFocus
  | InnerRight_SelectFocus
  | OuterRightSelectFocus

derive instance Generic SelectFocus _

instance Show SelectFocus where
  show x = genericShow x

instance Eq SelectFocus where
  eq x = genericEq x

--------------------------------------------------------------------------------

areSiblings :: Point -> Point -> Boolean
areSiblings (Point is0 _j0) (Point is1 _j1) = is0 == is1

areOrderedSiblings :: Point -> Point -> Boolean
areOrderedSiblings (Point is0 j0) (Point is1 j1) = (is0 == is1) && (j0 <= j1)

orderSiblings :: Point -> Point -> Maybe (Point /\ Point)
orderSiblings p0@(Point _ j0) p1@(Point _ j1) | areSiblings p0 p1 =
  if j0 <= j1 then
    Just $ p0 /\ p1
  else
    Just $ p1 /\ p0
orderSiblings _ _ = Nothing

getHandleFromTo :: Handle -> Handle -> Maybe Handle
-- drag from a Point to a Point
getHandleFromTo (Cursor_Handle (Cursor _ p0 Left_CursorFocus)) (Cursor_Handle (Cursor p1 p1_ _)) | p1 == p1_, areOrderedSiblings p1 p0 = Just $ Cursor_Handle (Cursor p1 p0 Left_CursorFocus)
getHandleFromTo (Cursor_Handle (Cursor p0 _ Right_CursorFocus)) (Cursor_Handle (Cursor p1 p1_ _)) | p1 == p1_, areOrderedSiblings p0 p1 = Just $ Cursor_Handle (Cursor p0 p1 Right_CursorFocus)
-- TODO: other cases
getHandleFromTo _ _ = Nothing
