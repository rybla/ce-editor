module Data.Expr where

import Prelude

import Control.Alternative (guard)
import Control.Plus (empty)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Foldable (fold, foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (assert, brackets, bug, extractAt_Array, extractSpan_Array, impossible, insertSpanAt_Array, parens, spaces, todo)

--------------------------------------------------------------------------------

data Expr = Expr Label (Array Expr)

infix 0 Expr as %

derive instance Generic Expr _

instance Show Expr where
  show (l % []) = show l
  show (l % es) = parens $ Array.intercalate " " ([ show l, "%" ] <> (es # map show))

instance Eq Expr where
  eq x = genericEq x

--------------------------------------------------------------------------------

newtype Span = Span (Array Expr)

derive instance Generic Span _

derive instance Newtype Span _

instance Show Span where
  show x = genericShow x

--------------------------------------------------------------------------------

data Label
  = Root
  | String String

derive instance Generic Label _

instance Show Label where
  show Root = "Root"
  show (String s) = show s

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

derive instance Ord Path

derive newtype instance Semigroup Path

derive newtype instance Monoid Path

cons_Path :: Step -> Path -> Path
cons_Path i (Path is) = Path (i : is)

infixr 6 cons_Path as |:

uncons_Path ∷ Path → Maybe { head ∷ Step, tail ∷ Path }
uncons_Path (Path is) = List.uncons is <#> \{ head, tail } -> { head, tail: Path tail }

unsnoc_Path ∷ Path → Maybe { init ∷ Path, last ∷ Step }
unsnoc_Path (Path is) = List.unsnoc is <#> \{ init, last } -> { init: Path init, last }

stripPrefix_Path :: Path -> Path -> Path
stripPrefix_Path (Path Nil) is' = is'
stripPrefix_Path (Path (i : is)) (Path (i' : is')) | i == i' = stripPrefix_Path (Path is) (Path is')
stripPrefix_Path is is' = bug $ "stripPrefix_Path " <> show is <> " " <> show is'

modifyDescendant_Expr :: Path -> (Expr -> Expr) -> Expr -> Expr
modifyDescendant_Expr path0 f e0 = go identity path0 e0
  where
  go wrap path e = case uncons_Path path of
    Nothing -> wrap $ f e
    Just { head: i, tail: path' } ->
      go
        (\e' -> wrap $ e # modifyKid_Expr i (const e'))
        path'
        (e # getKid_Expr i)

modifyDescendant_Span :: Cursor -> (Span -> Span) -> Expr -> Expr
modifyDescendant_Span cursor f e0 = go identity (getPath_Cursor cursor) e0
  where
  go :: (Expr -> Expr) -> Path -> Expr -> Expr
  go wrap path e = case uncons_Path path of
    Nothing ->
      wrap
        $ (\es -> Expr (getLabel_Expr e) (unwrap es))
        $ Span (extract.before <> unwrap (f (Span extract.at)) <> extract.after)
      where
      indices = cursor # getIndices_Cursor
      extract = e # getKids_Expr # extractSpan_Array (unwrap indices.left) (unwrap indices.right)
    Just { head: i, tail: path' } ->
      go
        (\e' -> wrap $ e # modifyKid_Expr i (const e'))
        path'
        (e # getKid_Expr i)

getLabel_Expr ∷ Expr → Label
getLabel_Expr (Expr l _) = l

getKid_Expr :: Step -> Expr -> Expr
getKid_Expr i (Expr _ es) = es Array.!! unwrap i # fromMaybe' impossible

getKids_Expr :: Expr -> Array Expr
getKids_Expr (Expr _ es) = es

modifyKid_Expr :: Step -> (Expr -> Expr) -> Expr -> Expr
modifyKid_Expr i f (Expr l es) = Expr l $ Array.modifyAt (unwrap i) f es # fromMaybe' impossible

getKidsBetweenIndices :: Index -> Index -> Expr -> Array Expr
getKidsBetweenIndices (Index j_L) (Index j_R) es = es # getKids_Expr # Array.take j_R # Array.drop j_L

getKidsBetweenIndicesExcludingStep :: Index -> Index -> Step -> Expr -> Array Expr
getKidsBetweenIndicesExcludingStep j_L j_R i e =
  e
    # getKidsBetweenIndices j_L j_R
    # Array.deleteAt (unwrap i)
    # fromMaybe' impossible

--------------------------------------------------------------------------------
-- Step
--------------------------------------------------------------------------------

newtype Step = Step Int

derive instance Newtype Step _

instance Show Step where
  show (Step i) = show i

derive instance Eq Step

derive instance Ord Step

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

newtype Index = Index Int

derive instance Newtype Index _

instance Show Index where
  show (Index i) = show i

derive instance Eq Index

derive instance Ord Index

derive newtype instance Semiring Index

derive newtype instance Ring Index

getFirstIndex_Expr :: Expr -> Index
getFirstIndex_Expr _ = Index 0

getLastIndex_Expr :: Expr -> Index
getLastIndex_Expr e = Index (e # getKids_Expr # Array.length)

getExtremeIndices :: Expr -> { left :: Index, right :: Index }
getExtremeIndices (Expr _ es) = { left: Index 0, right: Index (Array.length es) }

--------------------------------------------------------------------------------
-- Step and Index Utilities
--------------------------------------------------------------------------------

orderedStepAndIndex :: Step -> Index -> Boolean
orderedStepAndIndex (Step i) (Index j) = i < j

-- the `|` corresponds to a step and the `.` corresponds to an index
infixl 4 orderedStepAndIndex as |<.

orderedIndexAndStep :: Index -> Step -> Boolean
orderedIndexAndStep (Index j) (Step i) = j <= i

-- the `|` corresponds to a step and the `.` corresponds to an index
infixl 4 orderedIndexAndStep as .<|

getIndicesAroundStep :: Step -> { left :: Index, right :: Index }
getIndicesAroundStep (Step i) = { left: Index i, right: Index (i + 1) }

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- the List Int is the steps along Expr kis
-- the final Int is the final point index in the last Expr's kids
data Point = Point Path Index

derive instance Generic Point _

instance Show Point where
  -- show x = genericShow x
  show (Point is j) = parens $ show is <> " △ " <> show j

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare x = genericCompare x

getPath :: Point -> Path
getPath (Point is _) = is

getIndex :: Point -> Index
getIndex (Point _ j) = j

insertAtIndex :: Index -> Span -> Expr -> Expr
insertAtIndex j (Span es') (Expr l es) = Expr l (before <> es' <> after)
  where
  { before, after } = Array.splitAt (unwrap j) es

insertAtPoint :: Point -> Span -> Expr -> Expr
insertAtPoint (Point path j) span e@(Expr l es) = case uncons_Path path of
  Nothing -> insertAtIndex j span e
  Just { head, tail } -> Expr l (es # Array.modifyAt (unwrap head) (insertAtPoint (Point tail j) span) # fromMaybe' impossible)

--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- | The values are:
-- |   - path_O: outer path
-- |   - j_OL: outer left point
-- |   - j_OR: outer right point
-- |   - path_I: inner path
-- |   - j_IL: inner left point
-- |   - j_IR: inner right point
data Handle = Handle Path Index Index Path Index Index HandleFocus

derive instance Generic Handle _

instance Show Handle where
  -- show x = genericShow x
  show h | Just p <- toPointHandle h =
    spaces [ "[[", show p, "]]" ]
  show h | Just c <- toCursorHandle h =
    spaces [ "[[", show c, "]]" ]
  show (Handle path_O j_OL j_OR path_I j_IL j_IR f) =
    spaces [ "[[", show path_O, "|", show j_OL, "…", show j_OR, "|", show path_I, "|", show j_IL, "…", show j_IR, "@", show f, "]]" ]

instance Eq Handle where
  eq x = genericEq x

-- Handle path_O j_OL j_OR path_I j_IL j_IR f
validHandle :: Handle -> Boolean
validHandle (Handle _path_O j_OL j_OR path_I j_IL j_IR _) =
  case unwrap path_I of
    Nil ->
      (j_OL <= j_IL && j_IL <= j_IR && j_IR <= j_OR)
    i : _ ->
      (j_OL .<| i && i |<. j_OR) &&
        (j_IL <= j_IR)

mkHandle :: Path -> Index -> Index -> Path -> Index -> Index -> HandleFocus -> Handle
mkHandle path_O j_OL j_OR path_I j_IL j_IR f =
  let
    h = Handle path_O j_OL j_OR path_I j_IL j_IR f
  in
    if
      not $ validHandle h then
      bug $ "invalid Handle: " <> show h
    else
      h

mkHandle' :: { path_O :: Path, j_OL :: Index, j_OR :: Index, path_I :: Path, j_IL :: Index, j_IR :: Index, f :: HandleFocus } -> Handle
mkHandle' { path_O, j_OL, j_OR, path_I, j_IL, j_IR, f } = mkHandle path_O j_OL j_OR path_I j_IL j_IR f

mkPointHandle :: Point -> Handle
mkPointHandle (Point is j) = mkHandle is j j (Path Nil) j j InnerLeft_HandleFocus

mkCursorHandle :: Cursor -> Handle
mkCursorHandle (Cursor is l r Left_CursorFocus) = mkHandle is l r (Path Nil) l l InnerLeft_HandleFocus
mkCursorHandle (Cursor is l r Right_CursorFocus) = mkHandle is l r (Path Nil) r r InnerRight_HandleFocus

getHandleFocus :: Handle -> HandleFocus
getHandleFocus (Handle _ _ _ _ _ _ f) = f

getHandlePoints :: Handle -> Point /\ Point /\ Point /\ Point
getHandlePoints (Handle path_O j_OL j_OR path_I j_IL j_IR _) =
  let
    p_OL = Point path_O j_OL
    p_IL = Point (path_O <> path_I) j_IL
    p_IR = Point (path_O <> path_I) j_IR
    p_OR = Point path_O j_OR
  in
    p_OL /\ p_IL /\ p_IR /\ p_OR

data HandleFocus
  = OuterLeft_HandleFocus
  | InnerLeft_HandleFocus
  | InnerRight_HandleFocus
  | OuterRight_HandleFocus

derive instance Generic HandleFocus _

instance Show HandleFocus where
  -- show x = genericShow x
  show OuterLeft_HandleFocus = "OL"
  show InnerLeft_HandleFocus = "IL"
  show InnerRight_HandleFocus = "IR"
  show OuterRight_HandleFocus = "OR"

instance Eq HandleFocus where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

data Cursor = Cursor Path Index Index CursorFocus

mkCursor' :: { f :: CursorFocus, j_L :: Index, j_R :: Index, path :: Path } -> Cursor
mkCursor' { path, j_L, j_R, f } = Cursor path j_L j_R f

getPath_Cursor (Cursor path _ _ _) = path
getLeftIndex_Cursor (Cursor _ j_L _ _) = j_L
getRightIndex_Cursor (Cursor _ _ j_R _) = j_R
getIndices_Cursor (Cursor _ j_L j_R _) = { left: j_L, right: j_R }

derive instance Generic Cursor _

instance Show Cursor where
  -- show x = genericShow x
  show (Cursor is l r f) = show is <> " △ " <> show l <> " … " <> show r <> " @ " <> show f

instance Eq Cursor where
  eq x = genericEq x

getCursorPoints :: Cursor -> Point /\ Point
getCursorPoints (Cursor is l r _) = Point is l /\ Point is r

getLeftPoint :: Cursor -> Point
getLeftPoint (Cursor is l _ _) = Point is l

getRightPoint :: Cursor -> Point
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
toPointHandle (Handle path_O j_OL j_OR path_I j_IL j_IR _) = do
  guard $ j_OL == j_OR && path_I == Path Nil && j_IL == j_IR
  pure $ Point path_O j_OL

toCursorHandle :: Handle -> Maybe Cursor
toCursorHandle h@(Handle path_O j_OL j_OR path_I j_IL_IR _j_IR f) = do
  guard $ path_I == Path Nil && j_IL_IR == _j_IR
  pure $ Cursor path_O j_OL j_OR
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
getDragOrigin h p | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, p == l = mkCursorHandle $ mkCursor' { path: getPath p, j_L: getIndex p, j_R: getIndex r, f: Left_CursorFocus }
getDragOrigin h p | Just c <- toCursorHandle h, l /\ r <- getCursorPoints c, p == r = mkCursorHandle $ mkCursor' { path: getPath p, j_L: getIndex l, j_R: getIndex p, f: Right_CursorFocus }
getDragOrigin h p | Just c <- toCursorHandle h = mkPointHandle p
-- starting from a point of existing Handle
-- TODO: choose prioritization for this in cases when two points of Handle are equal
getDragOrigin h@(Handle path_O j_OL j_OR path_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_OL = mkHandle' { path_O, j_OL, j_OR, path_I, j_IL, j_IR, f: OuterLeft_HandleFocus }
getDragOrigin h@(Handle path_O j_OL j_OR path_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_IL = mkHandle' { path_O, j_OL, j_OR, path_I, j_IL, j_IR, f: InnerLeft_HandleFocus }
getDragOrigin h@(Handle path_O j_OL j_OR path_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_IR = mkHandle' { path_O, j_OL, j_OR, path_I, j_IL, j_IR, f: InnerRight_HandleFocus }
getDragOrigin h@(Handle path_O j_OL j_OR path_I j_IL j_IR _) p | p_OL /\ p_IL /\ p_IR /\ p_OR <- getHandlePoints h, p == p_OR = mkHandle' { path_O, j_OL, j_OR, path_I, j_IL, j_IR, f: OuterRight_HandleFocus }
-- by default, if can't use h, just make a PointHandle
getDragOrigin _ p = mkPointHandle p

getHandleFromTo :: Handle -> Point -> Expr -> Maybe Handle

-- drag from a Point to a Point
--   - drag from a Point to the same Point
getHandleFromTo h p' e | Just p <- toPointHandle h, p == p' = pure $ mkPointHandle p'

--   - drag from a Point to a sibling Point
--     - drag from a Point to a Left sibling Point
getHandleFromTo h p_R e | Just p_L <- toPointHandle h, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_L) (getIndex p_L) (getIndex p_R) Right_CursorFocus
--     - drag from a Point to a Right sibling Point
getHandleFromTo h p_L e | Just p_R <- toPointHandle h, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_R) (getIndex p_L) (getIndex p_R) Left_CursorFocus

--   - drag from an inner Point to an outer Point
--     - drag from a inner left Point to an outer left Point
getHandleFromTo h p_OL e | Just p_I <- toPointHandle h, Just (i /\ path_I') <- isAncestorSibling p_OL p_I, getIndex p_OL .<| i = do
  let path_O = getPath p_OL
  let path_I = i |: path_I'
  pure $ mkHandle'
    { path_O
    , j_OL: getIndex p_OL
    , j_OR: (getIndicesAroundStep i).right
    , path_I
    , j_IL: getIndex p_I
    , j_IR: (getDescendant (path_O <> path_I) e # getExtremeIndices).right
    , f: OuterLeft_HandleFocus
    }
--     - drag from a inner right Point to an outer right Point
getHandleFromTo h p_OR e | Just p_I <- toPointHandle h, Just (i /\ path_I') <- isAncestorSibling p_OR p_I, i |<. getIndex p_OR = do
  let path_O = getPath p_OR
  let path_I = i |: path_I'
  pure $ mkHandle'
    { path_O
    , j_OL: (getIndicesAroundStep i).left
    , j_OR: getIndex p_OR
    , path_I
    , j_IL: (getDescendant (path_O <> path_I) e # getExtremeIndices).left
    , j_IR: getIndex p_I
    , f: OuterRight_HandleFocus
    }

--   - drag from an outer Point to an inner Point
--     - drag from an outer left Point to an inner left Point
getHandleFromTo h p_I e | Just p_OL <- toPointHandle h, Just (i /\ path_I') <- isAncestorSibling p_OL p_I, getIndex p_OL .<| i = do
  let path_O = getPath p_OL
  let path_I = i |: path_I'
  pure $ mkHandle'
    { path_O
    , j_OL: getIndex p_OL
    , j_OR: (getIndicesAroundStep i).right
    , path_I
    , j_IL: getIndex p_I
    , j_IR: (getDescendant (path_O <> path_I) e # getExtremeIndices).right
    , f: InnerLeft_HandleFocus
    }
--     - drag from an outer right Point to an inner right Point
getHandleFromTo h p_I e | Just p_OR <- toPointHandle h, Just (i /\ path_I') <- isAncestorSibling p_OR p_I, i |<. getIndex p_OR = do
  let path_O = getPath p_OR
  let path_I = i |: path_I'
  pure $ mkHandle'
    { path_O: getPath p_OR
    , j_OL: (getIndicesAroundStep i).left
    , j_OR: getIndex p_OR
    , path_I: i |: path_I'
    , j_IL: (getDescendant (path_O <> path_I) e # getExtremeIndices).left
    , j_IR: getIndex p_I
    , f: InnerRight_HandleFocus
    }

-- drag from a Cursor
getHandleFromTo h p_R e | Just c <- toCursorHandle h, p_L <- getCursorAnchorPoint c, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_R) (getIndex p_L) (getIndex p_R) Right_CursorFocus
getHandleFromTo h p_L e | Just c <- toCursorHandle h, p_R <- getCursorAnchorPoint c, areOrderedSiblings p_L p_R = pure $ mkCursorHandle $ Cursor (getPath p_L) (getIndex p_L) (getIndex p_R) Left_CursorFocus

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
getHandleFromTo _ _ _ = empty

--------------------------------------------------------------------------------
-- Tooth
--------------------------------------------------------------------------------

data Tooth = Tooth Label (Array Expr) Index

instance Show Tooth where
  show t = showTooth' t "{{}}"

showTooth' :: Tooth -> String -> String
showTooth' (Tooth l es i) s =
  parens $ "Tooth " <> show l <> " " <>
    brackets (((es_L # map show) <> [ s ] <> (es_R # map show)) # Array.intercalate " ")
  where
  { before: es_L, after: es_R } = Array.splitAt (unwrap i) es

mkTooth :: Label -> Array Expr -> Index -> Tooth
mkTooth l es i = Tooth l es i
  where
  _ = assert "Tooth index is in range" (0 <= unwrap i && unwrap i < Array.length es)

unTooth :: Tooth -> Span -> Expr
-- unTooth (Tooth l es i) span = Expr l $ Array.insertAt (unwrap i) e es # fromMaybe' impossible
unTooth (Tooth l es i) span = Expr l $ insertSpanAt_Array (unwrap i) (unwrap span) es

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

data Zipper = Zipper (Array Expr) Index (List Tooth)

instance Show Zipper where
  show (Zipper es i ts) =
    "{{ "
      <> (es_L # map show # Array.intercalate " ")
      <> foldr showTooth' "{{}}" ts
      <> (es_R # map show # Array.intercalate " ")
      <> " }}"
    where
    { before: es_L, after: es_R } = Array.splitAt (unwrap i) es

unZipper :: Zipper -> Span -> Span
unZipper (Zipper es i ts) span = Span $ es_L <> es' <> es_R
  where
  { before: es_L, after: es_R } = Array.splitAt (unwrap i) es
  es' = foldr (\t span' -> pure $ unTooth t (wrap span')) (unwrap span) ts

--------------------------------------------------------------------------------
-- Fragment
--------------------------------------------------------------------------------

data Fragment
  = Span_Fragment Span
  | Zipper_Fragment Zipper

derive instance Generic Fragment _

instance Show Fragment where
  show x = genericShow x

getFragment :: Handle -> Expr -> Fragment
getFragment h _ | Just p <- toPointHandle h = Span_Fragment $ Span []
getFragment h e | Just c <- toCursorHandle h = Span_Fragment $ getSpan c e
getFragment h e = Zipper_Fragment $ getZipper h e

getDescendant :: Path -> Expr -> Expr
getDescendant p e = case uncons_Path p of
  Nothing -> e
  Just { head, tail } -> getDescendant tail (e # getKid_Expr head)

getSpan :: Cursor -> Expr -> Span
getSpan (Cursor p j_L j_R f) e@(Expr _ es) = case uncons_Path p of
  Nothing -> Span $ Array.slice (unwrap j_L) (unwrap j_R) es
  Just { head, tail } -> getSpan (Cursor tail j_L j_R f) (e # getKid_Expr head)

getZipper :: Handle -> Expr -> Zipper
getZipper (Handle path_O j_OL j_OR path_I j_IL j_IR _f) e0 =
  let
    e1 = e0 # getDescendant path_O
  in
    case path_I # uncons_Path of
      -- path_I is empty
      Nothing ->
        Zipper
          ((e1 # getKidsBetweenIndices j_OL j_IL) <> (e1 # getKidsBetweenIndices j_IR j_OR))
          (j_IL - j_OL)
          Nil
      -- path_I is non-empty
      Just { head: i_I, tail: path_I' } ->
        Zipper
          (e1 # getKidsBetweenIndicesExcludingStep j_OL j_OR i_I)
          ((i_I # getIndicesAroundStep).left)
          (go Nil path_I' (e1 # getKid_Expr i_I))
        where
        go ths path e = case path # uncons_Path of
          Just { head: i, tail: path' } -> go (Tooth (e # getLabel_Expr) (es_L <> es_R) (i # getIndicesAroundStep).left : ths) path' e''
            where
            { before: es_L, at: e'', after: es_R } = e # getKids_Expr # extractAt_Array (unwrap i) # fromMaybe' impossible
          Nothing ->
            Tooth
              (e # getLabel_Expr)
              ((e # getKidsBetweenIndices (getFirstIndex_Expr e) j_IL) <> (e # getKidsBetweenIndices j_IR (getLastIndex_Expr e)))
              j_IL
              : ths

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

