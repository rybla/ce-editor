module Data.Expr where

import Prelude

import Control.Alternative (guard)
import Control.Plus (empty)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (bug, parens, todo)

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
  -- show x = genericShow x
  show (Point is j) = parens $ show (Array.fromFoldable is) <> " - " <> show j

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare x = genericCompare x

getPath :: Point -> List Int
getPath (Point is _) = is

getIndex :: Point -> Int
getIndex (Point _ j) = j

data Handle = Handle (List Int) Int Int (List Int) Int Int HandleFocus

derive instance Generic Handle _

instance Show Handle where
  show x = genericShow x

-- show (Point_Handle p) = "[[ " <> show p <> " ]]"
-- show (Cursor_Handle c) = "[[ " <> show c <> " ]]"
-- show (Select_Handle s) = "[[ " <> show s <> " ]]"

instance Eq Handle where
  eq x = genericEq x

-- Handle is_O j_OL j_OR is_I j_IL j_IR f

validHandle :: Handle -> Boolean
validHandle (Handle is_O j_OL j_OR is_I j_IL j_IR f) =
  case is_I of
    Nil ->
      (j_OL <= j_IL && j_IL <= j_IR && j_IR <= j_OR)
    i : _ ->
      (j_OL < j_IL && j_OL <= i && i <= j_IL) &&
        (j_IL <= j_IR)

mkHandle :: List Int -> Int -> Int -> List Int -> Int -> Int -> HandleFocus -> Handle
mkHandle is_O j_OL j_OR is_I j_IL j_IR f =
  let
    h = Handle is_O j_OL j_OR is_I j_IL j_IR f
  in
    if
      not $ validHandle h then
      bug $ "invalid Handle: " <> show h
    else
      h

mkPointHandle :: Point -> Handle
mkPointHandle (Point is j) = mkHandle is j j Nil j j InnerLeft_HandleFocus

mkCursorHandle :: Cursor -> Handle
mkCursorHandle (Cursor is l r Left_CursorFocus) = mkHandle is l r Nil l l InnerLeft_HandleFocus
mkCursorHandle (Cursor is l r Right_CursorFocus) = mkHandle is l r Nil r r InnerRight_HandleFocus

data HandleFocus
  = OuterLeft_HandleFocus
  | InnerLeft_HandleFocus
  | InnerRight_HandleFocus
  | OuterRightHandleFocus

derive instance Generic HandleFocus _

instance Show HandleFocus where
  show x = genericShow x

instance Eq HandleFocus where
  eq x = genericEq x

data Cursor = Cursor (List Int) Int Int CursorFocus

derive instance Generic Cursor _

instance Show Cursor where
  -- show (Cursor l r f) = show l <> " ... " <> show r <> " @ " <> show f
  show x = genericShow x

instance Eq Cursor where
  eq x = genericEq x

getPointsOfCursor (Cursor is l r _) = Point is l /\ Point is r
getLeftPoint (Cursor is l _ _) = Point is l
getRightPoint (Cursor is _ r _) = Point is r

data CursorFocus = Left_CursorFocus | Right_CursorFocus

derive instance Generic CursorFocus _

instance Show CursorFocus where
  show Left_CursorFocus = "Left"
  show Right_CursorFocus = "Right"

-- show x = genericShow x

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

-- if p0 is a sibling of an ancestor of p1, then computes:
--   - the index into the parent of p0 that goes down towards p1
--   - the path that when appended to the end of p0's path is the path of p1
isAncestorSibling :: Point -> Point -> Maybe (Int /\ List Int)
isAncestorSibling p0 p1 = go (getPath p0) (getPath p1)
  where
  go Nil (i1 : is1') = pure $ i1 /\ is1'
  go (i0 : is0') (i1 : is1') | i0 == i1 = go is0' is1'
  go _ _ = empty

getSelectFromPointToPoint :: Point -> Point -> Maybe Select
getSelectFromPointToPoint p0 p1
  | is1 <- getPath p1
  , j1 <- getIndex p1
  , Just (k0 /\ _is) <- isAncestorSibling p1 p0 =
      if k0 < getIndex p1 then
        pure $ Select (Point is1 (j1 - 1)) p0 p0 p1 OuterRightSelectFocus
      else
        pure $ Select p1 p0 p0 (Point is1 (j1 + 1)) OuterLeft_SelectFocus
getSelectFromPointToPoint _ _ = empty

toPointHandle :: Handle -> Maybe Point
toPointHandle (Handle is_O j_OL j_OR is_I j_IL j_IR _) = do
  guard $ j_OL == j_OR && is_I == Nil && j_IL == j_IR
  pure $ Point is_O j_OL

toCursorHandle :: Handle -> Maybe Cursor
toCursorHandle h@(Handle is_O j_OL j_OR is_I j_IL j_IR f) = do
  guard $ is_I == Nil && j_IL == j_IR
  let j_I = j_IL -- == j_IR
  pure $ Cursor is_O j_OL j_OR
    if j_OL == j_I then
      case f of
        OuterRightHandleFocus -> Right_CursorFocus
        _ -> Left_CursorFocus
    else if j_OR == j_I then
      case f of
        OuterLeft_HandleFocus -> Left_CursorFocus
        _ -> Right_CursorFocus
    else
      bug $ "invalid Handle: " <> show h

getHandleFromTo
  :: Handle
  -> Point
  -> Maybe Handle
-- -- drag from a Point
-- getHandleFromTo (Point_Handle p) p'
getHandleFromTo h p' | Just p <- toPointHandle h = case unit of
  _ | p == p' -> pure $ mkPointHandle p'
  _ | areOrderedSiblings p p' -> pure $ mkCursorHandle $ Cursor (getPath p) (getIndex p) (getIndex p') Right_CursorFocus
  _ | areOrderedSiblings p' p -> pure $ mkCursorHandle $ Cursor (getPath p) (getIndex p') (getIndex p) Left_CursorFocus
  _ | otherwise -> empty
-- _ | areOrderedSiblings p p' -> pure $ Cursor_Handle $ Cursor p p' Right_CursorFocus
-- _ | areOrderedSiblings p' p -> pure $ Cursor_Handle $ Cursor p' p Left_CursorFocus
-- -- drag from a Cursor
-- getHandleFromTo (Cursor_Handle (Cursor l _r Right_CursorFocus)) p'
--   | l == p' = pure $ Point_Handle p'
--   | areOrderedSiblings l p' = pure $ Cursor_Handle $ Cursor l p' Right_CursorFocus
--   | areOrderedSiblings p' l = pure $ Cursor_Handle $ Cursor p' l Left_CursorFocus
--   -- the second point in the Cursor should actually be the right-most sibling of `l`, right? 
--   -- but maybe this is fine for now, since you can adjust the selection post-hoc
--   | Just s <- getSelectFromPointToPoint l p' = pure $ Select_Handle s
-- getHandleFromTo (Cursor_Handle (Cursor _ r Left_CursorFocus)) p'
--   | r == p' = pure $ Point_Handle p'
--   | areOrderedSiblings p' r = pure $ Cursor_Handle $ Cursor p' r Left_CursorFocus
--   | areOrderedSiblings r p' = pure $ Cursor_Handle $ Cursor r p' Right_CursorFocus
getHandleFromTo (Handle is_O j_OL j_OR is_I j_IL j_IR f) p = empty

getHandleFromTo (Handle is_O j_OL j_OR is_I j_IL j_IR f) p = empty
-- TODO
getHandleFromTo _ _ = empty
