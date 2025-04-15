module Data.Expr where

import Prelude

import Control.Alternative (guard)
import Control.Plus (empty)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe', isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (extractAt_Array, extractSpan_Array, impossible, parens, spaces)

--------------------------------------------------------------------------------

newtype Step = Step Int

derive instance Newtype Step _

instance Show Step where
  show (Step i) = "|" <> show i

derive instance Eq Step

derive instance Ord Step

derive newtype instance Semiring Step

derive newtype instance Ring Step

atStep :: forall l. Show l => Step -> Expr l -> { outside :: Tooth l, at :: Expr l }
atStep i (Expr e) = { outside: Tooth { l: e.l, kids_L, kids_R }, at }
  where
  { before: kids_L, at, after: kids_R } = e.kids # extractAt_Array (unwrap i)
    # fromMaybe' (impossible $ "atStep " <> show i <> " " <> show (Expr e))

atSteps ∷ ∀ l. Show l ⇒ Expr l → Array { at ∷ Expr l, outside ∷ Tooth l }
atSteps e = e # getSteps # map \i -> e # atStep i

--------------------------------------------------------------------------------

newtype Index = Index Int

derive instance Newtype Index _

instance Show Index where
  show (Index i) = "." <> show i

derive instance Eq Index

derive instance Ord Index

derive newtype instance Semiring Index

derive newtype instance Ring Index

atIndexSpan_Expr :: forall l. Index -> Index -> Expr l -> { outside :: SpanTooth l, at :: Span l }
atIndexSpan_Expr i_L i_R (Expr e) = { outside: SpanTooth { l: e.l, kids_L, kids_R }, at: Span es }
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

getStepsAroundIndex :: Index -> { _L :: Step, _R :: Step }
getStepsAroundIndex (Index i) = { _L: Step (i - 1), _R: Step i }

--------------------------------------------------------------------------------

newtype Expr l = Expr { l :: l, kids :: Array (Expr l) }

derive instance Generic (Expr l) _

derive instance Newtype (Expr l) _

instance Show l => Show (Expr l) where
  show (Expr e) | Array.null e.kids = show e.l
  show (Expr e) = parens $ Array.intercalate " " ([ show e.l, "%" ] <> (e.kids # map show))

instance Eq l => Eq (Expr l) where
  eq x = genericEq x

derive instance Functor Expr
derive instance Foldable Expr
derive instance Traversable Expr

mkExpr ∷ forall l. l → Array (Expr l) → Expr l
mkExpr l kids = Expr { l, kids }

infix 0 mkExpr as %

getKid_Expr :: forall l. Show l => Step -> Expr l -> Expr l
getKid_Expr (Step i) (Expr e) = e.kids Array.!! i # fromMaybe' (impossible $ "getKid_Expr " <> show (Expr e) <> " " <> show (Step i))

getExtremeIndexes :: forall l. Expr l -> { _L :: Index, _R :: Index }
getExtremeIndexes (Expr e) = { _L: Index 0, _R: Index (Array.length e.kids) }

getExtremeSteps :: forall l. Expr l -> Maybe { _L :: Step, _R :: Step }
getExtremeSteps (Expr e) | Array.null e.kids = Nothing
getExtremeSteps (Expr e) = Just { _L: Step 0, _R: Step (Array.length e.kids - 1) }

getSteps :: forall l. Expr l -> Array Step
getSteps (Expr e) = e.kids # mapWithIndex \i _ -> Step i

--------------------------------------------------------------------------------

newtype Span l = Span (Array (Expr l))

derive instance Generic (Span l) _

derive instance Newtype (Span l) _

instance Show l => Show (Span l) where
  show x = genericShow x

derive instance Functor Span
derive instance Foldable Span
derive instance Traversable Span

getKid_Span :: forall l. Show l => Step -> Span l -> Expr l
getKid_Span i (Span es) = es Array.!! unwrap i # fromMaybe' (impossible $ "getKid_Span " <> show i <> " " <> show (Span es))

atIndexSpan_Span :: forall l. Index -> Index -> Span l -> { _L :: Span l, _R :: Span l, at :: Span l }
atIndexSpan_Span i_L i_R (Span es) = { _L: Span left, _R: Span right, at: Span es }
  where
  { before: left, at: es, after: right } = extractSpan_Array (unwrap i_L) (unwrap i_R) es

offset_Span :: forall l. Span l -> Index
offset_Span (Span es) = Index $ es # Array.length

--------------------------------------------------------------------------------

type Path = List Step

show_Path :: Path -> String
show_Path steps = "[" <> (steps # map show # List.intercalate " ") <> "]"

showNePath :: NePath -> String
showNePath nepath = show_Path (nepath # fromNePath)

atSubExpr :: forall l. Show l => Path -> Expr l -> { outside :: List (Tooth l), at :: Expr l }
atSubExpr = go Nil
  where
  go ts path e = case path of
    i : path' -> go (t : ts) path' e'
      where
      { outside: t, at: e' } = e # atStep i
    Nil -> { outside: List.reverse ts, at: e }

isPrefix_Path :: Path -> Path -> Boolean
isPrefix_Path Nil _ = true
isPrefix_Path (i : is) (i' : is') | i == i' = isPrefix_Path is is'
isPrefix_Path _ _ = false

--------------------------------------------------------------------------------

type NePath = NonEmpty List Step

cons_NePath :: Step -> NePath -> NePath
cons_NePath s (s' :| path) = s :| (s' : path)

infixr 5 cons_NePath as :|*

fromNePath :: NePath -> Path
fromNePath = List.fromFoldable

toNePath :: Path -> Maybe NePath
toNePath Nil = Nothing
toNePath (s : path) = Just (s :| path)

--------------------------------------------------------------------------------

newtype Point = Point { path :: Path, j :: Index }

mkPoint path j = Point { path, j }

derive instance Generic Point _

derive instance Newtype Point _

instance Show Point where
  show (Point p) = parens $ show_Path p.path <> " △ " <> show p.j

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare x = genericCompare x

--------------------------------------------------------------------------------

newtype Tooth l = Tooth { l :: l, kids_L :: Array (Expr l), kids_R :: Array (Expr l) }

mkTooth l kids_L kids_R = Tooth { l, kids_L, kids_R }

derive instance Generic (Tooth l) _

derive instance Newtype (Tooth l) _

instance Show l => Show (Tooth l) where
  show t = "{{ " <> showTooth' t "{{}}" <> " }}"

derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

showTooth' :: forall l. Show l => (Tooth l) -> String -> String
showTooth' (Tooth t) s = parens $ Array.intercalate " " $ [ show t.l, "%" ] <> (t.kids_L # map show) <> [ s ] <> (t.kids_R # map show)

-- isRoot_Tooth :: forall l. (Tooth l) -> Boolean
-- isRoot_Tooth (Tooth t) = t.l == Root

unTooth :: forall l. Tooth l -> Expr l -> Expr l
unTooth (Tooth t) e = Expr { l: t.l, kids: t.kids_L <> [ e ] <> t.kids_R }

offset_Tooth :: forall l. Tooth l -> Index
offset_Tooth (Tooth t) = Index $ t.kids_L # Array.length

getStep :: forall l. Tooth l -> Step
getStep (Tooth t) = Step $ t.kids_L # Array.length

--------------------------------------------------------------------------------

newtype SpanTooth l = SpanTooth { l :: l, kids_L :: Array (Expr l), kids_R :: Array (Expr l) }

derive instance Generic (SpanTooth l) _

derive instance Newtype (SpanTooth l) _

instance Show l => Show (SpanTooth l) where
  show st = "{{ " <> showSpanTooth' st "{{}}" <> " }}"

derive instance Functor SpanTooth
derive instance Foldable SpanTooth
derive instance Traversable SpanTooth

showSpanTooth' :: forall l. Show l => (SpanTooth l) -> String -> String
showSpanTooth' (SpanTooth st) s = parens $ Array.intercalate " " $ [ show st.l, "%" ] <> (st.kids_L # map show) <> [ s ] <> (st.kids_R # map show)

unSpanTooth :: forall l. (SpanTooth l) -> Span l -> Expr l
unSpanTooth (SpanTooth b) s = Expr { l: b.l, kids: b.kids_L <> unwrap s <> b.kids_R }

offset_outer_SpanTooth :: forall l. (SpanTooth l) -> { _L :: Index, _R :: Index }
offset_outer_SpanTooth (SpanTooth st) = { _L: Index $ st.kids_L # Array.length, _R: Index $ st.kids_R # Array.length }

offset_inner_SpanTooth :: forall l. (SpanTooth l) -> Index
offset_inner_SpanTooth (SpanTooth st) = Index $ st.kids_L # Array.length

--------------------------------------------------------------------------------

newtype ExprContext l = ExprContext (List (Tooth l))

derive instance Generic (ExprContext l) _

derive instance Newtype (ExprContext l) _

instance Show l => Show (ExprContext l) where
  show ec = "{{ " <> showExprContext' ec "{{}}" <> " }}"

derive instance Functor ExprContext
derive instance Foldable ExprContext
derive instance Traversable ExprContext

showExprContext' :: forall l. Show l => (ExprContext l) -> String -> String
showExprContext' (ExprContext ts) s = foldr showTooth' s ts

unExprContext :: forall l. (ExprContext l) -> Expr l -> Expr l
unExprContext (ExprContext ts) e = foldr unTooth e ts

offset_inner_ExprContext :: forall l. (ExprContext l) -> Index
offset_inner_ExprContext (ExprContext ts) = ts # List.last # maybe (Index 0) offset_Tooth

getPath_ExprContext :: forall l. (ExprContext l) -> Path
getPath_ExprContext (ExprContext ts) = ts # map getStep

--------------------------------------------------------------------------------

newtype SpanContext l = SpanContext { _O :: ExprContext l, _I :: SpanTooth l }

derive instance Generic (SpanContext l) _

derive instance Newtype (SpanContext l) _

instance Show l => Show (SpanContext l) where
  show (SpanContext sc@{ _O: ExprContext ts }) = "{{ " <> foldr showTooth' (show sc._I) ts <> " }}"

derive instance Functor SpanContext
derive instance Foldable SpanContext
derive instance Traversable SpanContext

showSpanContext' :: forall l. Show l => (SpanContext l) -> String -> String
showSpanContext' (SpanContext sc) s = showExprContext' sc._O $ showSpanTooth' sc._I s

unSpanContext :: forall l. (SpanContext l) -> Span l -> Expr l
unSpanContext (SpanContext sc) s = unExprContext sc._O $ unSpanTooth sc._I s

offset_inner_SpanContext :: forall l. (SpanContext l) -> Index
offset_inner_SpanContext (SpanContext sc) = sc._I # offset_inner_SpanTooth

getPath_SpanContext :: forall l. (SpanContext l) -> Path
getPath_SpanContext (SpanContext sc) = sc._O # getPath_ExprContext

--------------------------------------------------------------------------------

newtype Zipper l = Zipper { kids_L :: Array (Expr l), kids_R :: Array (Expr l), inside :: SpanContext l }

derive instance Newtype (Zipper l) _

instance Show l => Show (Zipper l) where
  show (Zipper z) =
    "{{ "
      <>
        ( [ z.kids_L # map show # Array.intercalate " "
          , showSpanContext' z.inside "{{}}"
          , z.kids_R # map show # Array.intercalate " "
          ] # Array.intercalate " "
        )
      <> " }}"

derive instance Functor Zipper
derive instance Foldable Zipper
derive instance Traversable Zipper

unZipper :: forall l. (Zipper l) -> Span l -> Span l
unZipper (Zipper z) s = Span $ z.kids_L <> [ unSpanContext z.inside s ] <> z.kids_R

offset_outer_Zipper :: forall l. (Zipper l) -> { _L :: Index, _R :: Index }
offset_outer_Zipper (Zipper z) = { _L: Index $ z.kids_L # Array.length, _R: Index $ z.kids_R # Array.length }

offset_inner_Zipper :: forall l. (Zipper l) -> Index
offset_inner_Zipper (Zipper z) = z.inside # offset_inner_SpanContext

--------------------------------------------------------------------------------

newtype SpanH = SpanH
  { path :: Path
  , j_L :: Index
  , j_R :: Index
  }

derive instance Generic SpanH _

derive instance Newtype SpanH _

instance Show SpanH where
  show (SpanH h) =
    spaces [ "[[", show_Path h.path, "|", show h.j_L, "…", show h.j_R, "]]" ]

instance Eq SpanH where
  eq x = genericEq x

getEndPoints_SpanH :: SpanH -> { _L :: Point, _R :: Point }
getEndPoints_SpanH (SpanH h) =
  { _L: Point { path: h.path, j: h.j_L }
  , _R: Point { path: h.path, j: h.j_R }
  }

atPoint :: forall l. Show l => Point -> Expr l -> { outside :: SpanContext l }
atPoint (Point p) e = { outside: at_span.outside }
  where
  at_span = atSpan (SpanH { path: p.path, j_L: p.j, j_R: p.j }) e

atSpan :: forall l. Show l => SpanH -> Expr l -> { outside :: SpanContext l, at :: Span l }
atSpan (SpanH h) e = { outside: SpanContext { _O: ExprContext at_path.outside, _I: at_span.outside }, at: at_span.at }
  where
  at_path = e # atSubExpr h.path
  at_span = at_path.at # atIndexSpan_Expr h.j_L h.j_R

--------------------------------------------------------------------------------

newtype ZipperH = ZipperH
  { path_O :: Path
  , j_OL :: Index
  , j_OR :: Index
  , path_I :: NePath
  , j_IL :: Index
  , j_IR :: Index
  }

derive instance Generic ZipperH _

derive instance Newtype ZipperH _

instance Show ZipperH where
  show (ZipperH h) =
    spaces [ "[[", show_Path h.path_O, "|", show h.j_OL, "…", show h.j_OR, "|", showNePath h.path_I, "|", show h.j_IL, "…", show h.j_IR, "]]" ]

instance Eq ZipperH where
  eq x = genericEq x

getEndPoints_ZipperH ∷ ZipperH → { _OL ∷ Point, _IL ∷ Point, _IR ∷ Point, _OR ∷ Point }
getEndPoints_ZipperH (ZipperH h) =
  { _OL: Point { path: h.path_O, j: h.j_OL }
  , _IL: Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_IL }
  , _IR: Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_IR }
  , _OR: Point { path: h.path_O, j: h.j_OR }
  }

getTotalInnerPath_ZipperH :: ZipperH -> NePath
getTotalInnerPath_ZipperH (ZipperH h) = case h.path_O of
  Nil -> h.path_I
  i : path_O -> (i :| path_O) <> h.path_I

atZipper :: forall l. Show l => ZipperH -> Expr l -> { outside :: SpanContext l, at :: Zipper l, inside :: Span l }
atZipper (ZipperH h) e =
  case h.path_I of
    -- Nil ->
    --   { outside: SpanContext { _O: ExprContext at_path_O.outside, _I: at_span_O.outside }
    --   , at: Zipper
    --       { kids_L: unwrap at_kid_M._L
    --       , kids_R: unwrap at_kid_M._R
    --       , inside: Nothing
    --       }
    --   , inside: at_kid_M.at
    --   }
    --   where
    --   at_kid_M = at_span_O.at # atIndexSpan_Span (h.j_IL - h.j_OL) (h.j_IR - h.j_OL)
    i_I :| path_I ->
      { outside: SpanContext { _O: ExprContext at_path_O.outside, _I: at_span_O.outside }
      , at: Zipper
          { kids_L: (unwrap at_kid_M.outside).kids_L
          , kids_R: (unwrap at_kid_M.outside).kids_R
          , inside: at_span_I.outside
          }
      , inside: at_span_I.at
      }
      where
      at_kid_M = at_path_O.at # atStep (i_I - (Step ((unwrap at_span_O.outside).kids_L # Array.length)))
      at_span_I = at_kid_M.at # atSpan (SpanH { j_L: h.j_IL, j_R: h.j_IR, path: path_I })
  where
  at_path_O = e # atSubExpr h.path_O
  at_span_O = at_path_O.at # atIndexSpan_Expr h.j_OL h.j_OR

--------------------------------------------------------------------------------

data Handle
  = Point_Handle Point
  | SpanH_Handle SpanH SpanFocus
  | ZipperH_Handle ZipperH ZipperFocus

derive instance Generic Handle _

instance Show Handle where
  show (Point_Handle p) = "[[ " <> show p <> " ]]"
  show (SpanH_Handle h focus) = show h <> " @ " <> show focus
  show (ZipperH_Handle h focus) = show h <> " @ " <> show focus

instance Eq Handle where
  eq x = genericEq x

data SpanFocus = Left_SpanFocus | Right_SpanFocus

derive instance Generic SpanFocus _

instance Eq SpanFocus where
  eq x = genericEq x

instance Show SpanFocus where
  show Left_SpanFocus = "L"
  show Right_SpanFocus = "R"

data ZipperFocus
  = OuterLeft_ZipperFocus
  | InnerLeft_ZipperFocus
  | InnerRight_ZipperFocus
  | OuterRight_ZipperFocus

derive instance Generic ZipperFocus _

instance Eq ZipperFocus where
  eq x = genericEq x

instance Show ZipperFocus where
  show OuterLeft_ZipperFocus = "OL"
  show InnerLeft_ZipperFocus = "IL"
  show InnerRight_ZipperFocus = "IR"
  show OuterRight_ZipperFocus = "OR"

defaultHandle :: Handle
defaultHandle = Point_Handle $ Point { path: Nil, j: Index 0 }

getFocusPoint :: Handle -> Point
getFocusPoint (Point_Handle p) = p
getFocusPoint (SpanH_Handle (SpanH h) Left_SpanFocus) = Point { path: h.path, j: h.j_L }
getFocusPoint (SpanH_Handle (SpanH h) Right_SpanFocus) = Point { path: h.path, j: h.j_R }
getFocusPoint (ZipperH_Handle (ZipperH h) OuterLeft_ZipperFocus) = Point { path: h.path_O, j: h.j_OL }
getFocusPoint (ZipperH_Handle (ZipperH h) InnerLeft_ZipperFocus) = Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_OL }
getFocusPoint (ZipperH_Handle (ZipperH h) InnerRight_ZipperFocus) = Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_OL }
getFocusPoint (ZipperH_Handle (ZipperH h) OuterRight_ZipperFocus) = Point { path: h.path_O, j: h.j_OR }

getOuterLeftPoint_Handle :: Handle -> Point
getOuterLeftPoint_Handle (Point_Handle p) = p
getOuterLeftPoint_Handle (SpanH_Handle sh _) = (sh # getEndPoints_SpanH)._L
getOuterLeftPoint_Handle (ZipperH_Handle zh _) = (zh # getEndPoints_ZipperH)._OL

--------------------------------------------------------------------------------

data Fragment l
  = Span_Fragment (Span l)
  | Zipper_Fragment (Zipper l)

derive instance Generic (Fragment l) _

instance Show l => Show (Fragment l) where
  show x = genericShow x

derive instance Functor Fragment
derive instance Foldable Fragment
derive instance Traversable Fragment

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- areSiblings_Point :: Point -> Point -> Boolean
-- areSiblings_Point (Point p0) (Point p1) = p0.path == p1.path

areSiblings_Point :: Point -> Point -> Maybe (Index /\ Index)
areSiblings_Point (Point p) (Point p') = do
  guard $ p.path == p'.path
  pure $ p.j /\ p'.j

areOrderedSiblings_Point :: Point -> Point -> Boolean
areOrderedSiblings_Point (Point p0) (Point p1) = (p0.path == p1.path) && (p0.j <= p1.j)

orderSiblings_Point :: Point -> Point -> Maybe (Point /\ Point)
orderSiblings_Point (Point p0) (Point p1) | areSiblings_Point (Point p0) (Point p1) # isJust =
  if p0.j <= p1.j then
    Just $ Point p0 /\ Point p1
  else
    Just $ Point p1 /\ Point p0
orderSiblings_Point _ _ = Nothing

-- | if p0 is a sibling of an ancestor of p1, then computes:
-- |   - the index into the parent of p0 that goes down towards p1
-- |   - the path that when appended to the end of p0's path is the path of p1
isAncestorSiblingOf_Point :: Point -> Point -> Maybe (Step /\ Path)
isAncestorSiblingOf_Point (Point p0) (Point p1) = go p0.path p1.path
  where
  go Nil (i1 : is1') = pure $ i1 /\ is1'
  go (i0 : is0') (i1 : is1') | i0 == i1 = go is0' is1'
  go _ _ = empty

