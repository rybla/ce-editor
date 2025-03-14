module Data.Expr where

import Prelude

import Control.Alternative (guard)
import Control.Plus (empty)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (brackets, bug, parens, spaces)

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
-- Path
--------------------------------------------------------------------------------

newtype Path = Path (List Step)

derive instance Newtype Path _

instance Show Path where
  show (Path is) = is
    # Array.fromFoldable
    # map show
    # Array.intercalate " | "
    # brackets

derive instance Eq Path

derive newtype instance Semigroup Path

derive newtype instance Monoid Path

consPath ∷ Step → Path → Path
consPath i (Path is) = Path (i : is)

infixr 6 consPath as |:

stripPrefixOfPath :: Path -> Path -> Path
stripPrefixOfPath (Path Nil) is' = is'
stripPrefixOfPath (Path (i : is)) (Path (i' : is')) | i == i' = stripPrefixOfPath (Path is) (Path is')
stripPrefixOfPath is is' = bug $ "stripPrefixOfPath " <> show is <> " " <> show is'

--------------------------------------------------------------------------------
-- Step
--------------------------------------------------------------------------------

newtype Step = Step Int

instance Show Step where
  show (Step i) = show i

derive instance Eq Step

derive instance Ord Step

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

newtype Index = Index Int

instance Show Index where
  show (Index i) = show i

derive instance Eq Index

derive instance Ord Index

derive newtype instance Semiring Index

--------------------------------------------------------------------------------
-- Step and Index Utilities
--------------------------------------------------------------------------------

orderedStepAndIndex :: Step -> Index -> Boolean
orderedStepAndIndex (Step i) (Index j) = i < j

-- the `|` corresponds to a step and the `.` corresponds to an index
infixl 4 orderedStepAndIndex as |<=.

orderedIndexAndStep :: Index -> Step -> Boolean
orderedIndexAndStep (Index j) (Step i) = i <= j

-- the `|` corresponds to a step and the `.` corresponds to an index
infixl 4 orderedIndexAndStep as .<=|

getIndicesAroundStep :: Step -> Index /\ Index
getIndicesAroundStep (Step i) = Index i /\ Index (i + 1)

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- the List Int is the steps along Expr kis
-- the final Int is the final point index in the last Expr's kids
data Point = Point Path Index

derive instance Generic Point _

instance Show Point where
  -- show x = genericShow x
  show (Point is j) = parens $ show is <> " - " <> show j

instance Eq Point where
  eq x = genericEq x

getPath :: Point -> Path
getPath (Point is _) = is

getIndex :: Point -> Index
getIndex (Point _ j) = j

--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

data Handle = Handle Path Index Index Path Index Index HandleFocus

derive instance Generic Handle _

instance Show Handle where
  -- show x = genericShow x
  show h | Just p <- toPointHandle h =
    spaces [ "[[", show p, "]]" ]
  show h | Just c <- toCursorHandle h =
    spaces [ "[[", show c, "]]" ]
  show (Handle is_O j_OL j_OR is_I j_IL j_IR f) =
    spaces [ "[[", show is_O, "|", show j_OL, "..", show j_OR, "|", show is_I, "|", show j_IL, "..", show j_IR, "@", show f, "]]" ]

instance Eq Handle where
  eq x = genericEq x

-- Handle is_O j_OL j_OR is_I j_IL j_IR f
validHandle :: Handle -> Boolean
validHandle (Handle _is_O j_OL j_OR is_I j_IL j_IR _) =
  case unwrap is_I of
    Nil ->
      (j_OL <= j_IL && j_IL <= j_IR && j_IR <= j_OR)
    i : _ ->
      (j_OL .<=| i && i |<=. j_OR) &&
        (j_IL <= j_IR)

mkHandle :: Path -> Index -> Index -> Path -> Index -> Index -> HandleFocus -> Handle
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
mkPointHandle (Point is j) = mkHandle is j j (Path Nil) j j InnerLeft_HandleFocus

mkCursorHandle :: Cursor -> Handle
mkCursorHandle (Cursor is l r Left_CursorFocus) = mkHandle is l r (Path Nil) l l InnerLeft_HandleFocus
mkCursorHandle (Cursor is l r Right_CursorFocus) = mkHandle is l r (Path Nil) r r InnerRight_HandleFocus

getHandleFocus ∷ Handle → HandleFocus
getHandleFocus (Handle _ _ _ _ _ _ f) = f

getHandlePoints :: Handle -> Point /\ Point /\ Point /\ Point
getHandlePoints (Handle is_O j_OL j_OR is_I j_IL j_IR _) =
  let
    p_OL = Point is_O j_OL
    p_IL = Point (is_O <> is_I) j_IL
    p_IR = Point (is_O <> is_I) j_IR
    p_OR = Point is_O j_OR
  in
    p_OL /\ p_IL /\ p_IR /\ p_OR

data HandleFocus
  = OuterLeft_HandleFocus
  | InnerLeft_HandleFocus
  | InnerRight_HandleFocus
  | OuterRight_HandleFocus

derive instance Generic HandleFocus _

instance Show HandleFocus where
  show x = genericShow x

instance Eq HandleFocus where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

data Cursor = Cursor Path Index Index CursorFocus

derive instance Generic Cursor _

instance Show Cursor where
  -- show x = genericShow x
  show (Cursor is l r f) = show is <> " - " <> show l <> " .. " <> show r <> " @ " <> show f

instance Eq Cursor where
  eq x = genericEq x

getCursorPoints ∷ Cursor → Point /\ Point
getCursorPoints (Cursor is l r _) = Point is l /\ Point is r

getLeftPoint ∷ Cursor → Point
getLeftPoint (Cursor is l _ _) = Point is l

getRightPoint ∷ Cursor → Point
getRightPoint (Cursor is _ r _) = Point is r

getCursorFocus :: Cursor -> CursorFocus
getCursorFocus (Cursor _ _ _ f) = f

getCursorAnchorPoint :: Cursor -> Point
getCursorAnchorPoint c = case f of
  Left_CursorFocus -> r
  Right_CursorFocus -> l
  where
  l /\ r = getCursorPoints c
  f = getCursorFocus c

getCursorFocusPoint :: Cursor -> Point
getCursorFocusPoint c = case f of
  Left_CursorFocus -> l
  Right_CursorFocus -> r
  where
  l /\ r = getCursorPoints c
  f = getCursorFocus c

data CursorFocus = Left_CursorFocus | Right_CursorFocus

derive instance Generic CursorFocus _

instance Show CursorFocus where
  show Left_CursorFocus = "Left"
  show Right_CursorFocus = "Right"

-- show x = genericShow x

instance Eq CursorFocus where
  eq x = genericEq x

-- getSelectFromPointToPoint :: Point -> Point -> Maybe Select
-- getSelectFromPointToPoint p0 p1
--   | is1 <- getPath p1
--   , j1 <- getIndex p1
--   , Just (k0 /\ _is) <- isAncestorSibling p1 p0 =
--       if k0 < getIndex p1 then
--         pure $ Select (Point is1 (j1 - 1)) p0 p0 p1 OuterRightSelectFocus
--       else
--         pure $ Select p1 p0 p0 (Point is1 (j1 + 1)) OuterLeft_SelectFocus
-- getSelectFromPointToPoint _ _ = empty

toPointHandle :: Handle -> Maybe Point
toPointHandle (Handle is_O j_OL j_OR is_I j_IL j_IR _) = do
  guard $ j_OL == j_OR && is_I == Path Nil && j_IL == j_IR
  pure $ Point is_O j_OL

toCursorHandle :: Handle -> Maybe Cursor
toCursorHandle h@(Handle is_O j_OL j_OR is_I j_IL_IR _j_IR f) = do
  guard $ is_I == Path Nil && j_IL_IR == _j_IR
  pure $ Cursor is_O j_OL j_OR
    if j_OL == j_IL_IR then
      case f of
        OuterRight_HandleFocus -> Right_CursorFocus
        _ -> Left_CursorFocus
    else if j_OR == j_IL_IR then
      case f of
        OuterLeft_HandleFocus -> Left_CursorFocus
        _ -> Right_CursorFocus
    else
      bug $ "invalid Handle: " <> show h

getDragOrigin :: Handle -> Point -> Handle
getDragOrigin h p | Just _ <- toPointHandle h = mkPointHandle p
-- starting from a point of existing Cursor Handle
getDragOrigin h p | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, p == l = mkCursorHandle $ Cursor (getPath p) (getIndex p) (getIndex r) Left_CursorFocus
getDragOrigin h p | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, p == r = mkCursorHandle $ Cursor (getPath p) (getIndex l) (getIndex p) Right_CursorFocus
getDragOrigin h p | Just c <- toCursorHandle h = mkPointHandle p
-- starting from a point of existing Handle
-- TODO: choose prioritization for this in cases when two points of Handle are equal
getDragOrigin h@(Handle is_O j_OL j_OR is_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_OL = Handle is_O j_OL j_OR is_I j_IL j_IR OuterLeft_HandleFocus
getDragOrigin h@(Handle is_O j_OL j_OR is_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_IL = Handle is_O j_OL j_OR is_I j_IL j_IR InnerLeft_HandleFocus
getDragOrigin h@(Handle is_O j_OL j_OR is_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_IR = Handle is_O j_OL j_OR is_I j_IL j_IR InnerRight_HandleFocus
getDragOrigin h@(Handle is_O j_OL j_OR is_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_OR = Handle is_O j_OL j_OR is_I j_IL j_IR OuterRight_HandleFocus
getDragOrigin _ p = mkPointHandle p

getHandleFromTo :: Handle -> Point -> Maybe Handle

-- drag from a Point to a Point
--   - drag from a Point to the same Point
getHandleFromTo h p' | Just p <- toPointHandle h, p == p' = pure $ mkPointHandle p'

--   - drag from a Point to a sibling Point
--     - drag from a Point to a Left sibling Point
getHandleFromTo h p_R | Just p_L <- toPointHandle h, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_L) (getIndex p_L) (getIndex p_R) Right_CursorFocus
--     - drag from a Point to a Right sibling Point
getHandleFromTo h p_L | Just p_R <- toPointHandle h, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_R) (getIndex p_L) (getIndex p_R) Left_CursorFocus

--   - drag from an inner Point to an outer Point
--     - drag from a inner left Point to an outer right Point
getHandleFromTo h p_OR | Just p_IL <- toPointHandle h, Just (i_L /\ is_I_mid) <- isAncestorSibling p_OR p_IL, i_L |<=. getIndex p_OR =
  -- let
  --   j_OL /\ _ = getIndicesAroundStep i_L
  -- in
  --   pure $ mkHandle (getPath p_OR) j_OL (getIndex p_OR) (i_L |: is_I_mid) (getIndex p_IL) (getIndex p_IL) OuterRight_HandleFocus
  let
    j_OL /\ _ = getIndicesAroundStep i_L
  in
    pure $ mkHandle (getPath p_OR) j_OL (getIndex p_OR) (i_L |: is_I_mid) (getIndex p_IL) (getIndex p_IL) OuterRight_HandleFocus

-- drag from a Cursor
getHandleFromTo h p_R | Just c <- toCursorHandle h, p_L <- getCursorAnchorPoint c, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_R) (getIndex p_L) (getIndex p_R) Right_CursorFocus
getHandleFromTo h p_L | Just c <- toCursorHandle h, p_R <- getCursorAnchorPoint c, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_L) (getIndex p_L) (getIndex p_R) Left_CursorFocus

-- -- drag from a Cursor
-- -- adjust left point of Cursor
-- -- getHandleFromTo h p_L | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, l == p_L = pure $ mkPointHandle p'
-- getHandleFromTo h p' | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, areOrderedSiblings l p' = pure $ mkCursorHandle $ Cursor (getPath p') (getIndex l) (getIndex p') Right_CursorFocus
-- getHandleFromTo h p' | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, areOrderedSiblings p' r = pure $ mkCursorHandle $ Cursor (getPath p') (getIndex p') (getIndex r) Left_CursorFocus
-- -- adjust right point of Cursor
-- -- getHandleFromTo h p' | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, r == p' = pure $ mkPointHandle p'
-- getHandleFromTo h p' | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, areOrderedSiblings p' r = pure $ mkCursorHandle $ Cursor (getPath p') (getIndex p') (getIndex r) Left_CursorFocus
-- getHandleFromTo h p' | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, areOrderedSiblings p' r = pure $ mkCursorHandle $ Cursor (getPath p') (getIndex l) (getIndex p') Right_CursorFocus

-- TODO
getHandleFromTo _ _ = empty

--------------------------------------------------------------------------------
-- Utilities
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

-- | if p0 is a sibling of an ancestor of p1, then computes:
-- |   - the index into the parent of p0 that goes down towards p1
-- |   - the path that when appended to the end of p0's path is the path of p1
isAncestorSibling :: Point -> Point -> Maybe (Step /\ Path)
isAncestorSibling p0 p1 = go (getPath p0) (getPath p1)
  where
  go (Path Nil) (Path (i1 : is1')) = pure $ i1 /\ Path is1'
  go (Path (i0 : is0')) (Path (i1 : is1')) | i0 == i1 = go (Path is0') (Path is1')
  go _ _ = empty

