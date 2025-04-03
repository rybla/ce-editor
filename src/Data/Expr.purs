module Data.Expr where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (brackets, bug, extractAt_Array, extractSpan_Array, impossible, parens, spaces)

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
  { before: kids_L, at, after: kids_R } = e.kids # extractAt_Array (unwrap i)
    # fromMaybe' (impossible $ "atStep: i is out of bounds: " <> show { i, e: Expr e })

--------------------------------------------------------------------------------

newtype Index = Index Int

derive instance Newtype Index _

instance Show Index where
  show (Index i) = "." <> show i

derive instance Eq Index

derive instance Ord Index

derive newtype instance Semiring Index

derive newtype instance Ring Index

atIndexSpan_Expr :: Index -> Index -> Expr -> { outside :: SpanTooth, at :: Span }
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
getStepsAroundIndex (Index i) = { _L: Step i, _R: Step (i + 1) }

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

mkExpr ∷ Label → Array Expr → Expr
mkExpr l kids = Expr { l, kids }

infix 0 mkExpr as %

getExtremeIndexes :: Expr -> { _L :: Index, _R :: Index }
getExtremeIndexes (Expr e) = { _L: Index 0, _R: Index (Array.length e.kids) }

--------------------------------------------------------------------------------

newtype Span = Span (Array Expr)

derive instance Generic Span _

derive instance Newtype Span _

instance Show Span where
  show x = genericShow x

getKid_Span :: Step -> Span -> Expr
getKid_Span i (Span es) = es Array.!! unwrap i # fromMaybe' (impossible "getKid_Span: i is out of bounds")

atIndexSpan_Span :: Index -> Index -> Span -> { _L :: Span, _R :: Span, at :: Span }
atIndexSpan_Span i_L i_R (Span es) = { _L: Span left, _R: Span right, at: Span es }
  where
  { before: left, at: es, after: right } = extractSpan_Array (unwrap i_L) (unwrap i_R) es

offset_Span :: Span -> Index
offset_Span (Span es) = Index $ es # Array.length

--------------------------------------------------------------------------------

newtype Path = Path (List Step)

derive instance Newtype Path _

instance Show Path where
  show (Path is) = is
    # Array.fromFoldable
    # map show
    # Array.intercalate " "
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

atSubExpr :: Path -> Expr -> { outside :: List Tooth, at :: Expr }
atSubExpr = go Nil
  where
  go ts path e = case path # uncons_Path of
    Just { head: i, tail: path' } -> go (t : ts) path' e'
      where
      { outside: t, at: e' } = e # atStep i
    Nothing -> { outside: List.reverse ts, at: e }

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
  show t = "{{ " <> showTooth' t "{{}}" <> " }}"

showTooth' :: Tooth -> String -> String
showTooth' (Tooth t) s = parens $ Array.intercalate " " $ [ show t.l, "%" ] <> (t.kids_L # map show) <> [ s ] <> (t.kids_R # map show)

isRoot_Tooth :: Tooth -> Boolean
isRoot_Tooth (Tooth t) = t.l == Root

unTooth :: Tooth -> Expr -> Expr
unTooth (Tooth t) e = Expr { l: t.l, kids: t.kids_L <> [ e ] <> t.kids_R }

offset_Tooth :: Tooth -> Index
offset_Tooth (Tooth t) = Index $ t.kids_L # Array.length

getStep_Tooth :: Tooth -> Step
getStep_Tooth (Tooth t) = Step $ t.kids_L # Array.length

--------------------------------------------------------------------------------

newtype SpanTooth = SpanTooth { l :: Label, kids_L :: Array Expr, kids_R :: Array Expr }

derive instance Generic SpanTooth _

derive instance Newtype SpanTooth _

instance Show SpanTooth where
  show st = "{{ " <> showSpanTooth' st "{{}}" <> " }}"

showSpanTooth' :: SpanTooth -> String -> String
showSpanTooth' (SpanTooth st) s = parens $ Array.intercalate " " $ [ show st.l, "%" ] <> (st.kids_L # map show) <> [ s ] <> (st.kids_R # map show)

unSpanTooth :: SpanTooth -> Span -> Expr
unSpanTooth (SpanTooth b) s = Expr { l: b.l, kids: b.kids_L <> unwrap s <> b.kids_R }

offset_outer_SpanTooth :: SpanTooth -> { _L :: Index, _R :: Index }
offset_outer_SpanTooth (SpanTooth st) = { _L: Index $ st.kids_L # Array.length, _R: Index $ st.kids_R # Array.length }

offset_inner_SpanTooth :: SpanTooth -> Index
offset_inner_SpanTooth (SpanTooth st) = Index $ st.kids_L # Array.length

--------------------------------------------------------------------------------

newtype ExprContext = ExprContext (List Tooth)

derive instance Generic ExprContext _

derive instance Newtype ExprContext _

instance Show ExprContext where
  show ec = "{{ " <> showExprContext' ec "{{}}" <> " }}"

showExprContext' :: ExprContext -> String -> String
showExprContext' (ExprContext ts) s = foldr showTooth' s ts

unExprContext :: ExprContext -> Expr -> Expr
unExprContext (ExprContext ts) e = foldr unTooth e ts

offset_inner_ExprContext :: ExprContext -> Index
offset_inner_ExprContext (ExprContext ts) = ts # List.last # maybe (Index 0) offset_Tooth

getPath_ExprContext :: ExprContext -> Path
getPath_ExprContext (ExprContext ts) = Path (ts # map getStep_Tooth)

--------------------------------------------------------------------------------

newtype SpanContext = SpanContext { _O :: ExprContext, _I :: SpanTooth }

derive instance Generic SpanContext _

derive instance Newtype SpanContext _

instance Show SpanContext where
  show (SpanContext sc@{ _O: ExprContext ts }) = "{{ " <> foldr showTooth' (show sc._I) ts <> " }}"

showSpanContext' :: SpanContext -> String -> String
showSpanContext' (SpanContext sc) s = showExprContext' sc._O $ showSpanTooth' sc._I s

unSpanContext :: SpanContext -> Span -> Expr
unSpanContext (SpanContext sc) s = unExprContext sc._O $ unSpanTooth sc._I s

offset_inner_SpanContext :: SpanContext -> Index
offset_inner_SpanContext (SpanContext sc) = sc._I # offset_inner_SpanTooth

--------------------------------------------------------------------------------

newtype Zipper = Zipper { kids_L :: Array Expr, kids_R :: Array Expr, inside :: Maybe SpanContext }

derive instance Newtype Zipper _

instance Show Zipper where
  show (Zipper z) =
    "{{ "
      <> (z.kids_L # map show # Array.intercalate " ")
      <>
        ( case z.inside of
            Nothing -> "{{}}"
            Just inside -> showSpanContext' inside "{{}}"
        )
      <> (z.kids_R # map show # Array.intercalate " ")
      <> " }}"

unZipper :: Zipper -> Span -> Span
unZipper (Zipper z@{ inside: Nothing }) s = Span $ z.kids_L <> unwrap s <> z.kids_R
unZipper (Zipper z@{ inside: Just inside }) s = Span $ z.kids_L <> [ unSpanContext inside s ] <> z.kids_R

offset_outer_Zipper :: Zipper -> { _L :: Index, _R :: Index }
offset_outer_Zipper (Zipper z) = { _L: Index $ z.kids_L # Array.length, _R: Index $ z.kids_R # Array.length }

offset_inner_Zipper :: Zipper -> Index
offset_inner_Zipper z@(Zipper { inside: Nothing }) = (z # offset_outer_Zipper)._L
offset_inner_Zipper (Zipper { inside: Just inside }) = inside # offset_inner_SpanContext

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
    spaces [ "[[", show h.path, "|", show h.j_L, "…", show h.j_R, "]]" ]

instance Eq SpanH where
  eq x = genericEq x

getEndPoints_SpanH :: SpanH -> { _L :: Point, _R :: Point }
getEndPoints_SpanH (SpanH h) =
  { _L: Point { path: h.path, j: h.j_L }
  , _R: Point { path: h.path, j: h.j_R }
  }

atPoint :: Point -> Expr -> { outside :: SpanContext }
atPoint (Point p) e = { outside: at_span.outside }
  where
  at_span = atSpan (SpanH { path: p.path, j_L: p.j, j_R: p.j }) e

atSpan :: SpanH -> Expr -> { outside :: SpanContext, at :: Span }
atSpan (SpanH h) e = { outside: SpanContext { _O: ExprContext at_path.outside, _I: at_span.outside }, at: at_span.at }
  where
  at_path = e # atSubExpr h.path
  at_span = at_path.at # atIndexSpan_Expr h.j_L h.j_R

--------------------------------------------------------------------------------

newtype ZipperH = ZipperH
  { path_O :: Path
  , j_OL :: Index
  , j_OR :: Index
  , path_I :: Path
  , j_IL :: Index
  , j_IR :: Index
  }

derive instance Generic ZipperH _

derive instance Newtype ZipperH _

instance Show ZipperH where
  show (ZipperH h) =
    spaces [ "[[", show h.path_O, "|", show h.j_OL, "…", show h.j_OR, "|", show h.path_I, "|", show h.j_IL, "…", show h.j_IR, "]]" ]

instance Eq ZipperH where
  eq x = genericEq x

getEndPoints_ZipperH ∷ ZipperH → { _OL ∷ Point, _IL ∷ Point, _IR ∷ Point, _OR ∷ Point }
getEndPoints_ZipperH (ZipperH h) =
  { _OL: Point { path: h.path_O, j: h.j_OL }
  , _IL: Point { path: h.path_O <> h.path_I, j: h.j_IL }
  , _IR: Point { path: h.path_O <> h.path_I, j: h.j_IR }
  , _OR: Point { path: h.path_O, j: h.j_OR }
  }

atZipper :: ZipperH -> Expr -> { outside :: SpanContext, at :: Zipper, inside :: Span }
atZipper (ZipperH h) e =
  case h.path_I # uncons_Path of
    Nothing ->
      { outside: SpanContext { _O: ExprContext at_path_O.outside, _I: at_span_O.outside }
      , at: Zipper
          { kids_L: unwrap at_kid_M._L
          , kids_R: unwrap at_kid_M._R
          , inside: Nothing
          }
      , inside: at_kid_M.at
      }
      where
      at_kid_M = at_span_O.at # atIndexSpan_Span (h.j_IL - h.j_OL) (h.j_IR - h.j_OL)
    Just { head: i_I, tail: path_I } ->
      { outside: SpanContext { _O: ExprContext at_path_O.outside, _I: at_span_O.outside }
      , at: Zipper
          { kids_L: (unwrap at_kid_M.outside).kids_L
          , kids_R: (unwrap at_kid_M.outside).kids_R
          , inside: Just at_span_I.outside
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

--------------------------------------------------------------------------------

getDragOrigin :: Handle -> Point -> Handle
getDragOrigin (SpanH_Handle h _) p | hp <- getEndPoints_SpanH h, p == hp._L = SpanH_Handle h Left_SpanFocus
getDragOrigin (SpanH_Handle h _) p | hp <- getEndPoints_SpanH h, p == hp._R = SpanH_Handle h Right_SpanFocus
getDragOrigin (ZipperH_Handle h _) p | hp <- getEndPoints_ZipperH h, p == hp._OL = ZipperH_Handle h OuterLeft_ZipperFocus
getDragOrigin (ZipperH_Handle h _) p | hp <- getEndPoints_ZipperH h, p == hp._IL = ZipperH_Handle h InnerLeft_ZipperFocus
getDragOrigin (ZipperH_Handle h _) p | hp <- getEndPoints_ZipperH h, p == hp._IR = ZipperH_Handle h InnerRight_ZipperFocus
getDragOrigin (ZipperH_Handle h _) p | hp <- getEndPoints_ZipperH h, p == hp._OR = ZipperH_Handle h OuterRight_ZipperFocus
getDragOrigin _ p = Point_Handle p

drag :: Handle -> Point -> Expr -> Maybe Handle

drag (Point_Handle (Point p)) (Point p') e = case unit of
  _ | Point p == Point p' -> pure $ Point_Handle (Point p')
  _ | areOrderedSiblings_Point (Point p) (Point p') -> pure $ SpanH_Handle (SpanH { path: p.path, j_L: p.j, j_R: p'.j }) Right_SpanFocus
  _ | areOrderedSiblings_Point (Point p') (Point p) -> pure $ SpanH_Handle (SpanH { path: p.path, j_L: p'.j, j_R: p.j }) Left_SpanFocus
  -- drag from inner left to outer left
  _ | p_IL <- p, p_OL <- p', Just (i /\ path_I') <- isAncestorSibling_Point (Point p_OL) (Point p_IL), p_OL.j .<| i -> do
    -- Debug.traceM $ "drag from inner left to outer left:\n  " <> show { p_IL, p_OL, i }
    let path_O = p'.path
    let path_I = i |: path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: p_OL.j
          , j_OR: i # getIndexesAroundStep # _._R
          , path_I
          , j_IL: p_IL.j
          , j_IR: e # atSubExpr (path_O <> path_I) # _.at # getExtremeIndexes # _._R
          }
      )
      OuterLeft_ZipperFocus
  -- drag from inner right to outer right
  _ | p_IR <- p, p_OR <- p', Just (i /\ path_I') <- isAncestorSibling_Point (Point p_OR) (Point p_IR), i |<. p_OR.j -> do
    let path_O = p_OR.path
    let path_I = i |: path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: i # getIndexesAroundStep # _._L
          , j_OR: p_OR.j
          , path_I
          , j_IL: e # atSubExpr (path_O <> path_I) # _.at # getExtremeIndexes # _._L
          , j_IR: p_IR.j
          }
      )
      OuterRight_ZipperFocus
  -- drag from outer left to inner left
  _ | p_OL <- p, p_IL <- p', Just (i /\ path_I') <- isAncestorSibling_Point (Point p_OL) (Point p_IL), p_OL.j .<| i -> do
    let path_O = p_OL.path
    let path_I = i |: path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: p_OL.j
          , j_OR: i # getIndexesAroundStep # _._R
          , path_I
          , j_IL: p_IL.j
          , j_IR: e # atSubExpr (path_O <> path_I) # _.at # getExtremeIndexes # _._R
          }
      )
      InnerLeft_ZipperFocus
  -- drag from outer right to inner right
  _ | p_OR <- p, p_IR <- p', Just (i /\ path_I') <- isAncestorSibling_Point (Point p_OR) (Point p_IR), i |<. p_OR.j -> do
    let path_O = p_OR.path
    let path_I = i |: path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: i # getIndexesAroundStep # _._L
          , j_OR: p_OR.j
          , path_I
          , j_IL: e # atSubExpr (path_O <> path_I) # _.at # getExtremeIndexes # _._L
          , j_IR: p_IR.j
          }
      )
      InnerRight_ZipperFocus
  _ | otherwise -> Nothing

drag (SpanH_Handle h focus) (Point p') _e = case focus of
  -- collapse to SpanH's Left Point
  Right_SpanFocus | hp._L == Point p' -> pure $ Point_Handle (Point p')
  -- adjust SpanH's Right Point
  Right_SpanFocus | areOrderedSiblings_Point hp._L (Point p') -> pure $ SpanH_Handle (SpanH { path: (unwrap hp._L).path, j_L: (unwrap hp._L).j, j_R: p'.j }) Right_SpanFocus
  -- drag focus to the left of SpanH's Left Point, which changes the focus to the Left
  Right_SpanFocus | areOrderedSiblings_Point (Point p') hp._L -> pure $ SpanH_Handle (SpanH { path: (unwrap hp._L).path, j_L: p'.j, j_R: (unwrap hp._L).j }) Left_SpanFocus
  -- collapse to SpanH's Right Point
  Left_SpanFocus | hp._R == Point p' -> pure $ Point_Handle (Point p')
  -- adjust SpanH's Left Point
  Left_SpanFocus | areOrderedSiblings_Point (Point p') hp._R -> pure $ SpanH_Handle (SpanH { path: (unwrap hp._R).path, j_L: p'.j, j_R: (unwrap hp._R).j }) Left_SpanFocus
  -- drag focus to right of SpanH's Right Point, which changes the focus to the Right
  Left_SpanFocus | areOrderedSiblings_Point hp._R (Point p') -> pure $ SpanH_Handle (SpanH { path: (unwrap hp._R).path, j_L: (unwrap hp._R).j, j_R: p'.j }) Right_SpanFocus
  _ | otherwise -> Nothing
  where
  hp = getEndPoints_SpanH h

drag (ZipperH_Handle h focus) _p' _e = case focus of
  _ | otherwise -> Nothing
  where
  _hp = getEndPoints_ZipperH h

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

