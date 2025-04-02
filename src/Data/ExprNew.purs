module Data.ExprNew where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (brackets, bug, extractAt_Array, extractSpan_Array, impossible, parens, spaces, todo)

--------------------------------------------------------------------------------

newtype Step = Step Int

derive instance Newtype Step _

instance Show Step where
  show (Step i) = "|" <> show i

derive instance Eq Step

derive instance Ord Step

derive newtype instance Semiring Step

derive newtype instance Ring Step

atStep :: Step -> Expr -> { outside :: Tooth, at :: Expr }
atStep i (Expr e) = { outside: Tooth { l: e.l, kids_L, kids_R }, at }
  where
  { before: kids_L, at, after: kids_R } = e.kids # extractAt_Array (unwrap i) # fromMaybe' impossible

--------------------------------------------------------------------------------

newtype Index = Index Int

derive instance Newtype Index _

instance Show Index where
  show (Index i) = "." <> show i

derive instance Eq Index

derive instance Ord Index

derive newtype instance Semiring Index

derive newtype instance Ring Index

atIndexSpan_Expr :: Index -> Index -> Expr -> { outside :: Tooth, at :: Span }
atIndexSpan_Expr i_L i_R (Expr e) = { outside: Tooth { l: e.l, kids_L, kids_R }, at: Span es }
  where
  { before: kids_L, at: es, after: kids_R } = extractSpan_Array (unwrap i_L) (unwrap i_R) e.kids

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

getIndexesAroundStep :: Step -> { _L :: Index, _R :: Index }
getIndexesAroundStep (Step i) = { _L: Index i, _R: Index (i + 1) }

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

newtype Expr = Expr { l :: Label, kids :: Array Expr }

derive instance Generic Expr _

derive instance Newtype Expr _

instance Show Expr where
  show (Expr e) | Array.null e.kids = show e.l
  show (Expr e) = parens $ Array.intercalate " " ([ show e.l, "%" ] <> (e.kids # map show))

instance Eq Expr where
  eq x = genericEq x

getFirstIndex_Expr :: Expr -> Index
getFirstIndex_Expr (Expr _) = Index 0

getLastIndex_Expr :: Expr -> Index
getLastIndex_Expr (Expr e) = Index (e.kids # Array.length)

getExtremeIndexes :: Expr -> { _L :: Index, _R :: Index }
getExtremeIndexes (Expr e) = { _L: Index 0, _R: Index (Array.length e.kids) }

--------------------------------------------------------------------------------

newtype Span = Span (Array Expr)

derive instance Generic Span _

derive instance Newtype Span _

instance Show Span where
  show x = genericShow x

getKid_Span :: Step -> Span -> Expr
getKid_Span i (Span es) = es Array.!! unwrap i # fromMaybe' impossible

atIndexSpan_Span :: Index -> Index -> Span -> { _L :: Span, _R :: Span, at :: Span }
atIndexSpan_Span i_L i_R (Span es) = { _L: Span left, _R: Span right, at: Span es }
  where
  { before: left, at: es, after: right } = extractSpan_Array (unwrap i_L) (unwrap i_R) es

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

atPath :: Path -> Expr -> { outside :: List Tooth, at :: Expr }
atPath = go Nil
  where
  go ts path e = case path # uncons_Path of
    Just { head: i, tail: path' } -> go (t : ts) path' e'
      where
      { outside: t, at: e' } = e # atStep i
    Nothing -> { outside: ts, at: e }

stripPrefix_Path :: Path -> Path -> Path
stripPrefix_Path (Path Nil) is' = is'
stripPrefix_Path (Path (i : is)) (Path (i' : is')) | i == i' = stripPrefix_Path (Path is) (Path is')
stripPrefix_Path is is' = bug $ "stripPrefix_Path " <> show is <> " " <> show is'

--------------------------------------------------------------------------------

newtype Point = Point { path :: Path, j :: Index }

mkPoint path j = Point { path, j }

derive instance Generic Point _

derive instance Newtype Point _

instance Show Point where
  show (Point p) = parens $ show p.path <> " △ " <> show p.j

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare x = genericCompare x

--------------------------------------------------------------------------------

newtype Tooth = Tooth { l :: Label, kids_L :: Array Expr, kids_R :: Array Expr }

mkTooth l kids_L kids_R = Tooth { l, kids_L, kids_R }

derive instance Generic Tooth _

derive instance Newtype Tooth _

instance Show Tooth where
  show t = showTooth' t "{{}}"

showTooth' :: Tooth -> String -> String
showTooth' (Tooth t) s = parens $ Array.intercalate " " $ [ show t.l, "%" ] <> (t.kids_L # map show) <> [ s ] <> (t.kids_R # map show)

isRoot_Tooth :: Tooth -> Boolean
isRoot_Tooth (Tooth t) = t.l == Root

unTooth :: Tooth -> Span -> Expr
unTooth (Tooth t) span = Expr { l: t.l, kids: t.kids_L <> unwrap span <> t.kids_R }

--------------------------------------------------------------------------------

newtype Context = Context (NonEmptyList Tooth)

derive instance Generic Context _

derive instance Newtype Context _

instance Show Context where
  show (Context ts) =
    "{{ " <> foldr showTooth' "{{}}" ts <> " }}"

unContext :: Context -> Span -> Expr
unContext (Context (NonEmptyList (t :| ts))) s = unTooth t $ foldr (\t' s' -> Span [ unTooth t' s' ]) s ts

--------------------------------------------------------------------------------

newtype Zipper = Zipper { kids_L :: Array Expr, kids_R :: Array Expr, ts :: List Tooth }

instance Show Zipper where
  show (Zipper z) =
    "{{ "
      <> (z.kids_L # map show # Array.intercalate " ")
      <> foldr showTooth' "{{}}" z.ts
      <> (z.kids_R # map show # Array.intercalate " ")
      <> " }}"

unZipper :: Zipper -> Span -> Span
unZipper (Zipper z) span = Span $ z.kids_L <> foldr (\t span' -> pure $ unTooth t (wrap span')) (unwrap span) z.ts <> z.kids_R

--------------------------------------------------------------------------------

newtype SpanHandle = SpanHandle
  { path :: Path
  , j_L :: Index
  , j_R :: Index
  }

derive instance Generic SpanHandle _

derive instance Newtype SpanHandle _

instance Show SpanHandle where
  show (SpanHandle h) =
    spaces [ "[[", show h.path, "|", show h.j_L, "…", show h.j_R, "]]" ]

instance Eq SpanHandle where
  eq x = genericEq x

atSpan :: SpanHandle -> Expr -> { ctx :: Context, at :: Span }
atSpan (SpanHandle h) e = { ctx: Context (NonEmptyList.snoc' at_path.outside at_span.outside), at: at_span.at }
  where
  at_path = e # atPath h.path
  at_span = at_path.at # atIndexSpan_Expr h.j_L h.j_R

--------------------------------------------------------------------------------

newtype ZipperHandle = ZipperHandle
  { path_O :: Path
  , j_OL :: Index
  , j_OR :: Index
  , path_I :: Path
  , j_IL :: Index
  , j_IR :: Index
  }

derive instance Generic ZipperHandle _

derive instance Newtype ZipperHandle _

instance Show ZipperHandle where
  show (ZipperHandle h) =
    spaces [ "[[", show h.path_O, "|", show h.j_OL, "…", show h.j_OR, "|", show h.path_I, "|", show h.j_IL, "…", show h.j_IR, "]]" ]

instance Eq ZipperHandle where
  eq x = genericEq x

atZipper :: ZipperHandle -> Expr -> { ctx :: Context, at :: Zipper, inside :: Span }
atZipper (ZipperHandle h) e =
  case h.path_I # uncons_Path of
    Nothing ->
      { ctx: Context (NonEmptyList.snoc' at_path_O.outside at_span_O.outside)
      , at: Zipper { kids_L: unwrap at_kid_M._L, kids_R: unwrap at_kid_M._R, ts: Nil }
      , inside: at_kid_M.at
      }
      where
      at_kid_M = at_span_O.at # atIndexSpan_Span (h.j_IL - h.j_OL) (h.j_IR - h.j_OL)
    Just { head: i_I, tail: path_I } ->
      { ctx: Context (NonEmptyList.snoc' at_path_O.outside at_span_O.outside)
      , at: Zipper
          { kids_L: (unwrap at_kid_M.outside).kids_L
          , kids_R: (unwrap at_kid_M.outside).kids_R
          , ts: at_span_I.ctx # unwrap # NonEmptyList.toList
          }
      , inside: at_span_I.at
      }
      where
      at_kid_M = at_path_O.at # atStep (i_I - (Step ((unwrap at_span_O.outside).kids_L # Array.length)))
      at_span_I = at_kid_M.at # atSpan (SpanHandle { j_L: h.j_IL, j_R: h.j_IR, path: path_I })
  where
  at_path_O = e # atPath h.path_O
  at_span_O = at_path_O.at # atIndexSpan_Expr h.j_OL h.j_OR

--------------------------------------------------------------------------------

data Handle
  = SpanHandle_Handle SpanHandle SpanHandleFocus
  | ZipperHandle_Handle ZipperHandle ZipperHandleFocus

data SpanHandleFocus = Left_SpanHandleFocus | Right_SpanHandleFocus

data ZipperHandleFocus
  = OuterLeft_ZipperHandleFocus
  | InnerLeft_ZipperHandleFocus
  | InnerRight_ZipperHandleFocus
  | OuterRight_ZipperHandleFocus

--------------------------------------------------------------------------------

getDragOrigin :: Handle -> Point -> Handle
getDragOrigin = todo ""

drag :: Handle -> Point -> Expr -> Maybe Handle
drag (SpanHandle_Handle h focus) p e = todo ""
drag (ZipperHandle_Handle h focus) p e = todo ""

--------------------------------------------------------------------------------

data Fragment
  = Span_Fragment Span
  | Zipper_Fragment Zipper

derive instance Generic Fragment _

instance Show Fragment where
  show x = genericShow x

-- --------------------------------------------------------------------------------

-- data HandleFocus
--   = OuterLeft_HandleFocus
--   | InnerLeft_HandleFocus
--   | InnerRight_HandleFocus
--   | OuterRight_HandleFocus

-- derive instance Generic HandleFocus _

-- instance Show HandleFocus where
--   -- show x = genericShow x
--   show OuterLeft_HandleFocus = "OL"
--   show InnerLeft_HandleFocus = "IL"
--   show InnerRight_HandleFocus = "IR"
--   show OuterRight_HandleFocus = "OR"

-- instance Eq HandleFocus where
--   eq x = genericEq x

-- --------------------------------------------------------------------------------

-- newtype Handle = Handle
--   { path_O :: Path
--   , j_OL :: Index
--   , j_OR :: Index
--   , path_I :: Path
--   , j_IL :: Index
--   , j_IR :: Index
--   , focus :: HandleFocus
--   }

-- derive instance Generic Handle _

-- derive instance Newtype Handle _

-- instance Show Handle where
--   show (Handle h) =
--     spaces [ "[[", show h.path_O, "|", show h.j_OL, "…", show h.j_OR, "|", show h.path_I, "|", show h.j_IL, "…", show h.j_IR, "@", show h.focus, "]]" ]

-- instance Eq Handle where
--   eq x = genericEq x

-- validHandle :: Handle -> Boolean
-- validHandle (Handle h) =
--   case unwrap h.path_I of
--     Nil ->
--       (h.j_OL <= h.j_IL && h.j_IL <= h.j_IR && h.j_IR <= h.j_OR)
--     i : _ ->
--       (h.j_OL .<| i && i |<. h.j_OR) &&
--         (h.j_IL <= h.j_IR)

-- mkHandle :: Path -> Index -> Index -> Path -> Index -> Index -> HandleFocus -> Handle
-- mkHandle path_O j_OL j_OR path_I j_IL j_IR focus =
--   let
--     h = Handle { path_O, j_OL, j_OR, path_I, j_IL, j_IR, focus }
--   in
--     if not $ validHandle h then
--       bug $ "invalid Handle: " <> show h
--     else
--       h

-- mkHandle_namedArgs { path_O, j_OL, j_OR, path_I, j_IL, j_IR, f } = mkHandle path_O j_OL j_OR path_I j_IL j_IR f

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

areSiblings_Point :: Point -> Point -> Boolean
areSiblings_Point (Point p0) (Point p1) = p0.path == p1.path

areOrderedSiblings_Point :: Point -> Point -> Boolean
areOrderedSiblings_Point (Point p0) (Point p1) = (p0.path == p1.path) && (p0.j <= p1.j)

orderSiblings_Point :: Point -> Point -> Maybe (Point /\ Point)
orderSiblings_Point (Point p0) (Point p1) | areSiblings_Point (Point p0) (Point p1) =
  if p0.j <= p1.j then
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

