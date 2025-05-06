module Data.Expr where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (ReaderT)
import Control.Plus (empty)
import Data.Array as Array
import Data.Diagnostic as Diagnostic
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldr, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe', isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as Ne
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Pretty (class Pretty, parens, pretty)
import Utility (extractAt_Array, extractSpan_Array, impossible, spaces)

--------------------------------------------------------------------------------

newtype Step = Step Int

derive instance Newtype Step _
derive instance Generic Step _

instance Show Step where
  show x = genericShow x

instance Pretty Step where
  pretty (Step i) = "|" <> show i

derive instance Eq Step

derive instance Ord Step

derive newtype instance Semiring Step

derive newtype instance Ring Step

rangeKidSteps :: forall l. Expr l -> Array Step
rangeKidSteps (Expr e) = e.kids # mapWithIndex \i -> const $ Step i

rangeSteps :: { _L :: Step, _R :: Step } -> Array Step
rangeSteps i = Array.range (i._L # unwrap) (i._R # unwrap) # map Step

atStep :: forall l. Show l => Step -> Expr l -> { outside :: Tooth l, here :: Expr l }
atStep i (Expr e) = { outside: Tooth { l: e.l, kids_L, kids_R }, here }
  where
  { before: kids_L, here, after: kids_R } = e.kids # extractAt_Array (unwrap i)
    # fromMaybe' (impossible $ "atStep " <> pretty i <> " " <> pretty (Expr e))

atSteps ∷ ∀ l. Show l ⇒ Expr l → Array { here :: Expr l, outside ∷ Tooth l }
atSteps e = e # getSteps # map \i -> e # atStep i

--------------------------------------------------------------------------------

newtype Index = Index Int

derive instance Newtype Index _
derive instance Generic Index _

instance Show Index where
  show x = genericShow x

instance Pretty Index where
  pretty (Index i) = "." <> show i

derive instance Eq Index

derive instance Ord Index

derive newtype instance Semiring Index

derive newtype instance Ring Index

rangeIndexes :: { _L :: Index, _R :: Index } -> Array Index
rangeIndexes i = Array.range (i._L # unwrap) (i._R # unwrap) # map Index

atIndexSpan_Expr :: forall l. Index -> Index -> Expr l -> { outside :: SpanTooth l, here :: Span l }
atIndexSpan_Expr i_L i_R (Expr e) = { outside: SpanTooth { l: e.l, kids_L, kids_R }, here: Span es }
  where
  { before: kids_L, here: es, after: kids_R } = extractSpan_Array (unwrap i_L) (unwrap i_R) e.kids

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
  show (Expr { l, kids }) = parens $ show l <> " % " <> show kids

instance Show l => Pretty (Expr l) where
  pretty (Expr e) | Array.null e.kids = show e.l
  pretty (Expr e) = parens $ Array.intercalate " " ([ show e.l, "%" ] <> (e.kids # map pretty))

instance Eq l => Eq (Expr l) where
  eq x = genericEq x

derive instance Functor Expr
derive instance Foldable Expr
derive instance Traversable Expr

mkExpr ∷ forall l. l → Array (Expr l) → Expr l
mkExpr l kids = Expr { l, kids }

infix 0 mkExpr as %

getKid_Expr :: forall l. Show l => Step -> Expr l -> Maybe (Expr l)
getKid_Expr (Step i) (Expr e) = e.kids Array.!! i

getExtremeIndexes :: forall l. Expr l -> { _L :: Index, _R :: Index }
getExtremeIndexes (Expr e) = { _L: Index 0, _R: Index (Array.length e.kids) }

getExtremeSteps :: forall l. Expr l -> Maybe { _L :: Step, _R :: Step }
getExtremeSteps (Expr e) | Array.null e.kids = Nothing
getExtremeSteps (Expr e) = Just { _L: Step 0, _R: Step (Array.length e.kids - 1) }

getSteps :: forall l. Expr l -> Array Step
getSteps (Expr e) = e.kids # mapWithIndex \i _ -> Step i

mapStepsAndKids :: forall l a. (Step -> Expr l -> a) -> Expr l -> Array a
mapStepsAndKids f (Expr e) = e.kids # mapWithIndex \i -> f (Step i)

traverseStepsAndKids :: forall l m a. Applicative m => (Step -> Expr l -> m a) -> Expr l -> m (Array a)
traverseStepsAndKids f (Expr e) = e.kids # traverseWithIndex \i -> f (Step i)

traverseIndexes :: forall l m a. Applicative m => (Index -> m a) -> Expr l -> m (Array a)
traverseIndexes f (Expr e) = rangeIndexes j # traverse f
  where
  j = Expr e # getExtremeIndexes

mapIndexes ∷ ∀ (l ∷ Type) (a ∷ Type). (Index → a) → Expr l → Array a
mapIndexes f (Expr e) = rangeIndexes j # map f
  where
  j = Expr e # getExtremeIndexes

--------------------------------------------------------------------------------

newtype Span l = Span (Array (Expr l))

derive instance Generic (Span l) _

derive instance Newtype (Span l) _

instance Show l => Show (Span l) where
  show x = genericShow x

instance Show l => Pretty (Span l) where
  pretty (Span es) = es # map pretty # String.joinWith " "

instance Eq l => Eq (Span l) where
  eq x = genericEq x

derive instance Functor Span
derive instance Foldable Span
derive instance Traversable Span

getKid_Span :: forall l. Show l => Step -> Span l -> Expr l
getKid_Span i (Span es) = es Array.!! unwrap i # fromMaybe' (impossible $ "getKid_Span " <> pretty i <> " " <> pretty (Span es))

atIndexSpan_Span :: forall l. Index -> Index -> Span l -> { _L :: Span l, _R :: Span l, here :: Span l }
atIndexSpan_Span i_L i_R (Span es) = { _L: Span left, _R: Span right, here: Span es }
  where
  { before: left, here: es, after: right } = extractSpan_Array (unwrap i_L) (unwrap i_R) es

offset_Span :: forall l. Span l -> Index
offset_Span (Span es) = Index $ es # Array.length

--------------------------------------------------------------------------------

type Path = List Step

pretty_Path :: Path -> String
pretty_Path steps = "[" <> (steps # map pretty # List.intercalate " ") <> "]"

pretty_NePath :: NePath -> String
pretty_NePath nepath = pretty_Path (nepath # fromNePath)

atSubExpr :: forall l. Show l => Path -> Expr l -> { outside :: List (Tooth l), here :: Expr l }
atSubExpr = go Nil
  where
  go ts path e = case path of
    i : path' -> go (t : ts) path' e'
      where
      { outside: t, here: e' } = e # atStep i
    Nil -> { outside: List.reverse ts, here: e }

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
  show x = genericShow x

instance Pretty Point where
  pretty (Point p) = parens $ pretty_Path p.path <> " △ " <> pretty p.j

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare x = genericCompare x

--------------------------------------------------------------------------------

newtype Tooth l = Tooth { l :: l, kids_L :: Array (Expr l), kids_R :: Array (Expr l) }

mkTooth l (kids_L /\ kids_R) = Tooth { l, kids_L, kids_R }

infix 0 mkTooth as %<

derive instance Generic (Tooth l) _

derive instance Newtype (Tooth l) _

instance Show l => Show (Tooth l) where
  show x = genericShow x

instance Show l => Pretty (Tooth l) where
  pretty t = "{{ " <> showTooth' t "{{}}" <> " }}"

derive newtype instance Eq l => Eq (Tooth l)

derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

showTooth' :: forall l. Show l => (Tooth l) -> String -> String
showTooth' (Tooth t) s = parens $ Array.intercalate " " $ [ show t.l, "%" ] <> (t.kids_L # map pretty) <> [ s ] <> (t.kids_R # map pretty)

-- isRoot_Tooth :: forall l. (Tooth l) -> Boolean
-- isRoot_Tooth (Tooth t) = t.l == Root

unTooth :: forall l. Tooth l -> Expr l -> Expr l
unTooth (Tooth t) e = Expr { l: t.l, kids: t.kids_L <> [ e ] <> t.kids_R }

offset_Tooth :: forall l. Tooth l -> Index
offset_Tooth (Tooth t) = Index $ t.kids_L # Array.length

getStep :: forall l. Tooth l -> Step
getStep (Tooth t) = Step $ t.kids_L # Array.length

mapStepsAndKids_Tooth :: forall l a. (Step -> Expr l -> a) -> a -> Tooth l -> Array a
mapStepsAndKids_Tooth f a (Tooth t) = (t.kids_L <#> Left) <> [ Right a ] <> (t.kids_R <#> Left) # mapWithIndex \i -> case _ of
  Left kid -> f (Step i) kid
  Right a' -> a'

getExtremeIndexes_Tooth ∷ ∀ (l ∷ Type). Tooth l → { _L ∷ Index, _R ∷ Index }
getExtremeIndexes_Tooth (Tooth t) = { _L: Index 0, _R: Index (length t.kids_L + 1 + length t.kids_R) }

mapIndexes_Tooth :: forall l a. (Index -> a) -> Tooth l -> Array a
mapIndexes_Tooth f tooth = rangeIndexes j # map f
  where
  j = tooth # getExtremeIndexes_Tooth

--------------------------------------------------------------------------------

newtype SpanTooth l = SpanTooth { l :: l, kids_L :: Array (Expr l), kids_R :: Array (Expr l) }

mkSpanTooth l (kids_L /\ kids_R) = SpanTooth { l, kids_L, kids_R }

infix 0 mkSpanTooth as %<*

derive instance Generic (SpanTooth l) _

derive instance Newtype (SpanTooth l) _

instance Show l => Show (SpanTooth l) where
  show x = genericShow x

instance Show l => Pretty (SpanTooth l) where
  pretty st = "{{ " <> showSpanTooth' st "{{}}" <> " }}"

derive newtype instance Eq l => Eq (SpanTooth l)

derive instance Functor SpanTooth
derive instance Foldable SpanTooth
derive instance Traversable SpanTooth

showSpanTooth' :: forall l. Show l => (SpanTooth l) -> String -> String
showSpanTooth' (SpanTooth st) s = parens $ Array.intercalate " " $ [ show st.l, "%" ] <> (st.kids_L # map pretty) <> [ s ] <> (st.kids_R # map pretty)

unSpanTooth :: forall l. (SpanTooth l) -> Span l -> Expr l
unSpanTooth (SpanTooth b) s = Expr { l: b.l, kids: b.kids_L <> unwrap s <> b.kids_R }

offset_outer_SpanTooth :: forall l. (SpanTooth l) -> { _L :: Index, _R :: Index }
offset_outer_SpanTooth (SpanTooth st) = { _L: Index $ st.kids_L # Array.length, _R: Index $ st.kids_R # Array.length }

offset_innerLeft_SpanTooth :: forall l. (SpanTooth l) -> Index
offset_innerLeft_SpanTooth (SpanTooth st) = Index $ st.kids_L # Array.length

offset_innerRight_SpanTooth :: forall l. (SpanTooth l) -> Index
offset_innerRight_SpanTooth (SpanTooth st) = Index $ st.kids_L # Array.length

mapStepsAndKids_SpanTooth :: forall l a. (Step -> Expr l -> a) -> Array a -> SpanTooth l -> Array a
mapStepsAndKids_SpanTooth f as (SpanTooth t) = (t.kids_L <#> Left) <> (Right <$> as) <> (t.kids_R <#> Left) # mapWithIndex \i -> case _ of
  Left kid -> f (Step i) kid
  Right a' -> a'

getExtremeIndexes_SpanTooth ∷ ∀ (l ∷ Type). Int -> SpanTooth l → { _L ∷ Index, _R ∷ Index }
getExtremeIndexes_SpanTooth n (SpanTooth t) = { _L: Index 0, _R: Index (length t.kids_L + n + length t.kids_R) }

mapIndexes_SpanTooth :: forall l a. Int -> (Index -> a) -> SpanTooth l -> Array a
mapIndexes_SpanTooth n f tooth = rangeIndexes j # map f
  where
  j = tooth # getExtremeIndexes_SpanTooth n

--------------------------------------------------------------------------------

newtype ExprContext l = ExprContext (List (Tooth l))

derive instance Generic (ExprContext l) _

derive instance Newtype (ExprContext l) _

instance Show l => Show (ExprContext l) where
  show x = genericShow x

instance Show l => Pretty (ExprContext l) where
  pretty ec = "{{ " <> showExprContext' ec "{{}}" <> " }}"

derive newtype instance Eq l => Eq (ExprContext l)

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
  show x = genericShow x

instance Show l => Pretty (SpanContext l) where
  pretty (SpanContext sc@{ _O: ExprContext ts }) = "{{ " <> foldr showTooth' (pretty sc._I) ts <> " }}"

derive newtype instance Eq l => Eq (SpanContext l)

derive instance Functor SpanContext
derive instance Foldable SpanContext
derive instance Traversable SpanContext

showSpanContext' :: forall l. Show l => (SpanContext l) -> String -> String
showSpanContext' (SpanContext sc) s = showExprContext' sc._O $ showSpanTooth' sc._I s

unSpanContext :: forall l. Show l => SpanContext l -> Span l -> Expr l
unSpanContext (SpanContext sc) s = unExprContext sc._O $ unSpanTooth sc._I s

offset_innerLeft_SpanContext :: forall l. (SpanContext l) -> Index
offset_innerLeft_SpanContext (SpanContext sc) = sc._I # offset_innerLeft_SpanTooth

offset_innerRight_SpanContext :: forall l. (SpanContext l) -> Index
offset_innerRight_SpanContext (SpanContext sc) = sc._I # offset_innerRight_SpanTooth

getPath_SpanContext :: forall l. (SpanContext l) -> Path
getPath_SpanContext (SpanContext sc) = sc._O # getPath_ExprContext

--------------------------------------------------------------------------------

newtype Zipper l = Zipper { kids_L :: Array (Expr l), kids_R :: Array (Expr l), inside :: SpanContext l }

derive instance Newtype (Zipper l) _
derive instance Generic (Zipper l) _

instance Show l => Show (Zipper l) where
  show x = genericShow x

instance Show l => Pretty (Zipper l) where
  pretty (Zipper z) =
    "{{ "
      <>
        ( [ z.kids_L # map pretty # Array.intercalate " "
          , showSpanContext' z.inside "{{}}"
          , z.kids_R # map pretty # Array.intercalate " "
          ] # Array.intercalate " "
        )
      <> " }}"

derive newtype instance Eq l => Eq (Zipper l)

derive instance Functor Zipper
derive instance Foldable Zipper
derive instance Traversable Zipper

unZipper :: forall l. Show l => Zipper l -> Span l -> Span l
unZipper (Zipper z) s = Span $ z.kids_L <> [ unSpanContext z.inside s ] <> z.kids_R

offset_outer_Zipper :: forall l. Zipper l -> { _L :: Index, _R :: Index }
offset_outer_Zipper (Zipper z) = { _L: Index $ z.kids_L # Array.length, _R: Index $ z.kids_R # Array.length }

offset_innerLeft_Zipper :: forall l. Zipper l -> Index
offset_innerLeft_Zipper (Zipper z) = z.inside # offset_innerLeft_SpanContext

offset_innerRight_Zipper :: forall l. Zipper l -> Index
offset_innerRight_Zipper (Zipper z) = z.inside # offset_innerRight_SpanContext

getPath_Zipper :: forall l. Zipper l -> Path
getPath_Zipper (Zipper z) = getPath_SpanContext z.inside

fromSpanContextToZipper :: forall l. SpanContext l -> Zipper l
fromSpanContextToZipper sc = Zipper
  { kids_L: []
  , kids_R: []
  , inside: sc
  }

--------------------------------------------------------------------------------

newtype SpanH = SpanH
  { path :: Path
  , j_L :: Index
  , j_R :: Index
  }

derive instance Generic SpanH _

derive instance Newtype SpanH _

instance Show SpanH where
  show x = genericShow x

instance Pretty SpanH where
  pretty (SpanH h) =
    spaces [ "[[", pretty_Path h.path, "|", pretty h.j_L, "…", pretty h.j_R, "]]" ]

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

atSpan :: forall l. Show l => SpanH -> Expr l -> { outside :: SpanContext l, here :: Span l }
atSpan (SpanH h) e = { outside: SpanContext { _O: ExprContext at_path.outside, _I: at_span.outside }, here: at_span.here }
  where
  at_path = e # atSubExpr h.path
  at_span = at_path.here # atIndexSpan_Expr h.j_L h.j_R

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
  show x = genericShow x

instance Pretty ZipperH where
  pretty (ZipperH h) =
    spaces [ "[[", pretty_Path h.path_O, "|", pretty h.j_OL, "…", pretty h.j_OR, "|", pretty_NePath h.path_I, "|", pretty h.j_IL, "…", pretty h.j_IR, "]]" ]

instance Eq ZipperH where
  eq x = genericEq x

getEndPoints_ZipperH ∷ ZipperH → { _OL ∷ Point, _IL ∷ Point, _IR ∷ Point, _OR ∷ Point }
getEndPoints_ZipperH (ZipperH h) =
  { _OL: Point { path: h.path_O, j: h.j_OL }
  , _IL: Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_IL }
  , _IR: Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_IR }
  , _OR: Point { path: h.path_O, j: h.j_OR }
  }

getOuterSpanH_ZipperH :: ZipperH -> SpanH
getOuterSpanH_ZipperH (ZipperH h) = SpanH { path: h.path_O, j_L: h.j_OL, j_R: h.j_OR }

getInnerSpanH_ZipperH :: ZipperH -> SpanH
getInnerSpanH_ZipperH (ZipperH h) = SpanH { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j_L: h.j_IL, j_R: h.j_IR }

getTotalInnerPath_ZipperH :: ZipperH -> NePath
getTotalInnerPath_ZipperH (ZipperH h) = case h.path_O of
  Nil -> h.path_I
  i : path_O -> (i :| path_O) <> h.path_I

atZipper :: forall l. Show l => ZipperH -> Expr l -> { outside :: SpanContext l, here :: Zipper l, inside :: Span l }
atZipper (ZipperH h) e =
  { outside: at_span_O.outside
  , here: Zipper
      { kids_L: at_span_M._L # unwrap
      , kids_R: at_span_M._R # unwrap
      , inside: at_span_I.outside
      }
  , inside: at_span_I.here
  }
  where
  at_expr_O = e # atSubExpr h.path_O
  at_span_O = e # atSpan (SpanH { path: h.path_O, j_L: h.j_OL, j_R: h.j_OR })

  j = h.path_I # Ne.head # getIndexesAroundStep
  at_span_M = at_span_O.here # atIndexSpan_Span (j._L - h.j_OL) (j._R - h.j_OL)

  at_span_I =
    at_expr_O.here
      # getKid_Expr (h.path_I # Ne.head)
      # fromMaybe' (impossible "index out of bounds")
      # atSpan (SpanH { path: h.path_I # Ne.tail, j_L: h.j_IL, j_R: h.j_IR })

--------------------------------------------------------------------------------

data Handle
  = Point_Handle Point
  | SpanH_Handle SpanH SpanFocus
  | ZipperH_Handle ZipperH ZipperFocus

derive instance Generic Handle _

instance Show Handle where
  show x = genericShow x

instance Pretty Handle where
  pretty (Point_Handle p) = "[[ " <> pretty p <> " ]]"
  pretty (SpanH_Handle h focus) = pretty h <> " @ " <> pretty focus
  pretty (ZipperH_Handle h focus) = pretty h <> " @ " <> pretty focus

instance Eq Handle where
  eq x = genericEq x

data SpanFocus = Left_SpanFocus | Right_SpanFocus

derive instance Generic SpanFocus _

instance Eq SpanFocus where
  eq x = genericEq x

instance Show SpanFocus where
  show x = genericShow x

instance Pretty SpanFocus where
  pretty Left_SpanFocus = "L"
  pretty Right_SpanFocus = "R"

data ZipperFocus
  = OuterLeft_ZipperFocus
  | InnerLeft_ZipperFocus
  | InnerRight_ZipperFocus
  | OuterRight_ZipperFocus

derive instance Generic ZipperFocus _

instance Eq ZipperFocus where
  eq x = genericEq x

instance Show ZipperFocus where
  show x = genericShow x

instance Pretty ZipperFocus where
  pretty OuterLeft_ZipperFocus = "OL"
  pretty InnerLeft_ZipperFocus = "IL"
  pretty InnerRight_ZipperFocus = "IR"
  pretty OuterRight_ZipperFocus = "OR"

defaultHandle :: Handle
defaultHandle = Point_Handle $ Point { path: Nil, j: Index 0 }

normalizeHandle :: Handle -> Handle
normalizeHandle (SpanH_Handle (SpanH h) _) | h.j_L == h.j_R = Point_Handle (Point { path: h.path, j: h.j_L })
-- normalizeHandle (ZipperH_Handle h f) = TODO: any rules for normalizing ZipperHs?
normalizeHandle h = h

getFocusPoint :: Handle -> Point
getFocusPoint (Point_Handle p) = p
getFocusPoint (SpanH_Handle (SpanH h) Left_SpanFocus) = Point { path: h.path, j: h.j_L }
getFocusPoint (SpanH_Handle (SpanH h) Right_SpanFocus) = Point { path: h.path, j: h.j_R }
getFocusPoint (ZipperH_Handle (ZipperH h) OuterLeft_ZipperFocus) = Point { path: h.path_O, j: h.j_OL }
getFocusPoint (ZipperH_Handle (ZipperH h) InnerLeft_ZipperFocus) = Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_IL }
getFocusPoint (ZipperH_Handle (ZipperH h) InnerRight_ZipperFocus) = Point { path: ZipperH h # getTotalInnerPath_ZipperH # fromNePath, j: h.j_IR }
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

instance Show l => Pretty (Fragment l) where
  pretty (Span_Fragment s) = pretty s
  pretty (Zipper_Fragment z) = pretty z

instance Eq l => Eq (Fragment l) where
  eq x = genericEq x

derive instance Functor Fragment
derive instance Foldable Fragment
derive instance Traversable Fragment

--------------------------------------------------------------------------------

data Diff l
  = Id_Diff
  | Inject_Diff (Array (Diff l))
  | DeleteTooth_Diff Step (Diff l)
  | InsertTooth_Diff (Tooth l) (Diff l)
  | ReplaceSpan_Diff Index Index (Span l)
  | Replace_Diff (Expr l)

derive instance Generic (Diff l) _

instance Show l => Show (Diff l) where
  show x = genericShow x

instance Show l => Pretty (Diff l) where
  pretty Id_Diff = "_"
  pretty (Inject_Diff kids) = "(_ % " <> (kids # map pretty # String.joinWith " ") <> ")"
  pretty (DeleteTooth_Diff i d) = "-(_ % ... -[" <> pretty i <> "]: " <> pretty d <> " ... )"
  pretty (InsertTooth_Diff t d) = "+(_ % ... +[" <> pretty (t # getStep) <> "]: " <> pretty d <> " ... )"
  pretty (ReplaceSpan_Diff j0 j1 span) = "//(_ % ... [" <> pretty j0 <> "] " <> (span # unwrap # map pretty # String.joinWith " ") <> " [" <> pretty j1 <> " ... )"
  pretty (Replace_Diff e) = "//" <> pretty e

atInjectDiff :: forall l l'. Show l => NePath -> (Expr l -> Diff l') -> Expr l -> Diff l'
atInjectDiff (i0 :| path0) f = goStep i0 path0
  where
  go :: Path -> Expr l -> Diff l'
  go path e = case path of
    i : path' -> goStep i path' e
    Nil -> f e

  goStep :: Step -> Path -> Expr l -> Diff l'
  goStep i path e = Inject_Diff $ e # mapStepsAndKids \i' e' ->
    if i /= i' then Id_Diff else go path e'

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

type EditMenu m l l' = String -> Array (String /\ Edit m l l')

type EditAt m l l' = PureEditorState l' -> Maybe (Edit m l l')

type EditM :: (Type -> Type) -> Type -> Type -> Type -> Type
type EditM m l l' = ReaderT (EditCtx m l l') (MaybeT (Diagnostic.MT m))

type EditCtx :: (Type -> Type) -> Type -> Type -> Type
type EditCtx m l l' =
  { liftLabel :: l -> m l'
  }

type Edit m l l' =
  Edit_ l
    ( EditM m l l'
        { root :: Expr l'
        , mb_handle :: Maybe Handle
        , clipboard :: Maybe (Fragment l')
        }
    )

data Edit_ l a = Edit
  { info :: EditInfo l
  , output :: Lazy a
  }

derive instance Generic (Edit_ l a) _

derive instance Functor (Edit_ l)

instance Show l => Show (Edit_ l a) where
  show x = x # map (const "_ :: Diagnostic") # genericShow

instance Show l => Pretty (Edit_ l a) where
  pretty x = show x

data EditInfo l
  = Insert_EditInfo { insertion :: Fragment l }
  | Remove_EditInfo {}
  | Copy_EditInfo {}

derive instance Generic (EditInfo l) _

instance Show l => Show (EditInfo l) where
  show x = genericShow x

instance Show l => Pretty (EditInfo l) where
  pretty x = show x

type PureEditorState l =
  { root :: Expr l
  , mb_handle :: Maybe Handle
  , clipboard :: Maybe (Fragment l)
  }

-- TODO: is this layer necessary? I used to merge with existing clipboard but that's already accounted for when the Edit is constructed, so no need to do it here
applyEdit :: forall m l l'. Monad m => Show l => Edit m l l' -> PureEditorState l' -> EditM m l l' (PureEditorState l')
applyEdit (Edit edit) _state = do
  state' <- edit.output # Lazy.force
  pure
    { root: state'.root
    , mb_handle: normalizeHandle <$> state'.mb_handle
    , clipboard: state'.clipboard
    }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

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

