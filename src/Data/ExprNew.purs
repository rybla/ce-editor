module Data.ExprNew where

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

newtype Step = Step Int

derive instance Newtype Step _

instance Show Step where
  show (Step i) = show i

derive instance Eq Step

derive instance Ord Step

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

data Expr = Expr Label (Array Expr)

infix 0 Expr as %

derive instance Generic Expr _

instance Show Expr where
  show (l % []) = show l
  show (l % es) = parens $ Array.intercalate " " ([ show l, "%" ] <> (es # map show))

instance Eq Expr where
  eq x = genericEq x

getLabel_Expr ∷ Expr → Label
getLabel_Expr (Expr l _) = l

getKid_Expr :: Step -> Expr -> Expr
getKid_Expr i (Expr _ es) = es Array.!! unwrap i # fromMaybe' impossible

getKids_Expr :: Expr -> Array Expr
getKids_Expr (Expr _ es) = es

--------------------------------------------------------------------------------

newtype Span = Span (Array Expr)

derive instance Generic Span _

derive instance Newtype Span _

instance Show Span where
  show x = genericShow x

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

--------------------------------------------------------------------------------

newtype Point = Point { path :: Path, index :: Index }

mkPoint path index = Point { path, index }

derive instance Generic Point _

derive instance Newtype Point _

instance Show Point where
  show (Point p) = parens $ show p.path <> " △ " <> show p.index

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare x = genericCompare x

--------------------------------------------------------------------------------

newtype Tooth = Tooth { label :: Label, kids_L :: Array Expr, kids_R :: Array Expr }

mkTooth label kids_L kids_R = Tooth { label, kids_L, kids_R }

derive instance Generic Tooth _

derive instance Newtype Tooth _

instance Show Tooth where
  show t = showTooth' t "{{}}"

showTooth' :: Tooth -> String -> String
showTooth' (Tooth t) s =
  parens $ "Tooth " <> show t.label <> " " <>
    brackets (((t.kids_L # map show) <> [ s ] <> (t.kids_R # map show)) # Array.intercalate " ")

unTooth :: Tooth -> Span -> Expr
unTooth (Tooth t) span = Expr t.label $ t.kids_L <> unwrap span <> t.kids_R

--------------------------------------------------------------------------------

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

data Handle = Handle
  { path_O :: Path
  , j_OL :: Index
  , j_OR :: Index
  , path_I :: Path
  , j_IL :: Index
  , j_IR :: Index
  , focus :: HandleFocus
  }

derive instance Generic Handle _

instance Show Handle where
  show (Handle h) =
    spaces [ "[[", show h.path_O, "|", show h.j_OL, "…", show h.j_OR, "|", show h.path_I, "|", show h.j_IL, "…", show h.j_IR, "@", show h.focus, "]]" ]

instance Eq Handle where
  eq x = genericEq x

-- Handle path_O j_OL j_OR path_I j_IL j_IR f
validHandle :: Handle -> Boolean
validHandle (Handle h) =
  case unwrap h.path_I of
    Nil ->
      (h.j_OL <= h.j_IL && h.j_IL <= h.j_IR && h.j_IR <= h.j_OR)
    i : _ ->
      (h.j_OL .<| i && i |<. h.j_OR) &&
        (h.j_IL <= h.j_IR)

mkHandle :: Path -> Index -> Index -> Path -> Index -> Index -> HandleFocus -> Handle
mkHandle path_O j_OL j_OR path_I j_IL j_IR focus =
  let
    h = Handle { path_O, j_OL, j_OR, path_I, j_IL, j_IR, focus }
  in
    if
      not $ validHandle h then
      bug $ "invalid Handle: " <> show h
    else
      h

mkHandle_namedArgs { path_O, j_OL, j_OR, path_I, j_IL, j_IR, f } = mkHandle path_O j_OL j_OR path_I j_IL j_IR f

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

newtype Zipper = Zipper { kids_L :: Array Expr, kids_R :: Array Expr, tooths :: List Tooth }

instance Show Zipper where
  show (Zipper z) =
    "{{ "
      <> (z.kids_L # map show # Array.intercalate " ")
      <> foldr showTooth' "{{}}" z.tooths
      <> (z.kids_R # map show # Array.intercalate " ")
      <> " }}"

unZipper :: Zipper -> Span -> Span
unZipper (Zipper z) span = Span $ z.kids_L <> es' <> z.kids_R
  where
  es' = foldr (\t span' -> pure $ unTooth t (wrap span')) (unwrap span) z.tooths

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

areSiblings_Point :: Point -> Point -> Boolean
areSiblings_Point (Point p0) (Point p1) = p0.path == p1.path

areOrderedSiblings_Point :: Point -> Point -> Boolean
areOrderedSiblings_Point (Point p0) (Point p1) = (p0.path == p1.path) && (p0.index <= p1.index)

orderSiblings_Point :: Point -> Point -> Maybe (Point /\ Point)
orderSiblings_Point (Point p0) (Point p1) | areSiblings_Point (Point p0) (Point p1) =
  if p0.index <= p1.index then
    Just $ Point p0 /\ Point p1
  else
    Just $ Point p1 /\ Point p0
orderSiblings_Point _ _ = Nothing

-- | if p0 is a sibling of an ancestor of p1, then computes:
-- |   - the index into the parent of p0 that goes down towards p1
-- |   - the path that when appended to the end of p0's path is the path of p1
isAncestorSibling_Point :: Point -> Point -> Maybe (Step /\ Path)
isAncestorSibling_Point (Point p0) (Point p1) = go p0.path p1.path
  where
  go (Path Nil) (Path (i1 : is1')) = pure $ i1 /\ Path is1'
  go (Path (i0 : is0')) (Path (i1 : is1')) | i0 == i1 = go (Path is0') (Path is1')
  go _ _ = empty

