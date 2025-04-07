module Data.Expr.Drag where

import Data.Expr
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
import Utility (brackets, bug, extractAt_Array, extractSpan_Array, impossible, parens, spaces, todo)

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
  --
  _ | otherwise -> Nothing
  where
  hp = getEndPoints_SpanH h

drag (ZipperH_Handle h focus) _p' _e = case focus of
  -- -- adjust ZipperH's Outer Left Point
  -- OuterLeft_ZipperFocus -> todo ""
  -- -- adjust ZipperH's Outer Right Point
  -- OuterRight_ZipperFocus -> todo ""
  -- -- adjust ZipperH's Inner Left Point
  -- InnerLeft_ZipperFocus -> todo ""
  -- -- adjust ZipperH's Inner Right Point
  -- InnerRight_ZipperFocus -> todo ""
  --
  _ | otherwise -> Nothing
  where
  _hp = getEndPoints_ZipperH h

