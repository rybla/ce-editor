module Data.Expr.Drag where

import Data.Expr
import Prelude

import Control.Alternative (guard)
import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.NonEmpty as Ne
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Utility (todo)

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
  _ | p_IL <- p, p_OL <- p', Just (i /\ path_I') <- isAncestorSiblingOf_Point (Point p_OL) (Point p_IL), p_OL.j .<| i -> do
    -- Debug.traceM $ "drag from inner left to outer left:\n  " <> show { p_IL, p_OL, i }
    let path_O = p'.path
    let path_I = i :| path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: p_OL.j
          , j_OR: i # getIndexesAroundStep # _._R
          , path_I
          , j_IL: p_IL.j
          , j_IR: e # atSubExpr (path_O <> (path_I # fromNePath)) # _.at # getExtremeIndexes # _._R
          }
      )
      OuterLeft_ZipperFocus
  -- drag from inner right to outer right
  _ | p_IR <- p, p_OR <- p', Just (i /\ path_I') <- isAncestorSiblingOf_Point (Point p_OR) (Point p_IR), i |<. p_OR.j -> do
    let path_O = p_OR.path
    let path_I = i :| path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: i # getIndexesAroundStep # _._L
          , j_OR: p_OR.j
          , path_I
          , j_IL: e # atSubExpr (path_O <> (path_I # fromNePath)) # _.at # getExtremeIndexes # _._L
          , j_IR: p_IR.j
          }
      )
      OuterRight_ZipperFocus
  -- drag from outer left to inner left
  _ | p_OL <- p, p_IL <- p', Just (i /\ path_I') <- isAncestorSiblingOf_Point (Point p_OL) (Point p_IL), p_OL.j .<| i -> do
    let path_O = p_OL.path
    let path_I = i :| path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: p_OL.j
          , j_OR: i # getIndexesAroundStep # _._R
          , path_I
          , j_IL: p_IL.j
          , j_IR: e # atSubExpr (path_O <> (path_I # fromNePath)) # _.at # getExtremeIndexes # _._R
          }
      )
      InnerLeft_ZipperFocus
  -- drag from outer right to inner right
  _ | p_OR <- p, p_IR <- p', Just (i /\ path_I') <- isAncestorSiblingOf_Point (Point p_OR) (Point p_IR), i |<. p_OR.j -> do
    let path_O = p_OR.path
    let path_I = i :| path_I'
    pure $ ZipperH_Handle
      ( ZipperH
          { path_O
          , j_OL: i # getIndexesAroundStep # _._L
          , j_OR: p_OR.j
          , path_I
          , j_IL: e # atSubExpr (path_O <> (path_I # fromNePath)) # _.at # getExtremeIndexes # _._L
          , j_IR: p_IR.j
          }
      )
      InnerRight_ZipperFocus
  _ | otherwise -> Nothing

drag (SpanH_Handle (SpanH h) focus) (Point p') e = case focus of
  Right_SpanFocus -> drag (Point_Handle hp._L) (Point p') e
  Left_SpanFocus -> drag (Point_Handle hp._R) (Point p') e
  where
  hp = SpanH h # getEndPoints_SpanH

drag (ZipperH_Handle (ZipperH h) focus) (Point p') e = case focus of
  -- adjust ZipperH's OuterLeft Point
  OuterLeft_ZipperFocus | isPrefix_Path p'.path h_path_I ->
    drag (Point_Handle hp._IL) (Point p') e >>= case _ of
      ZipperH_Handle (ZipperH h') focus' -> pure $ ZipperH_Handle
        ( ZipperH h'
            { j_IL = h.j_IL
            , j_IR = h.j_IR
            , j_OR = if areOrderedSiblings_Point (Point p') (Point { path: h.path_O, j: (h.path_I # Ne.head # getIndexesAroundStep)._L }) then h.j_OR else h'.j_OR
            }
        )
        focus'
      h' -> Just h'
  -- adjust ZipperH's OuterRight Point
  OuterRight_ZipperFocus | isPrefix_Path p'.path h_path_I ->
    drag (Point_Handle hp._IR) (Point p') e >>= case _ of
      ZipperH_Handle (ZipperH h') focus' -> pure $ ZipperH_Handle
        ( ZipperH h'
            { j_IL = h.j_IL
            , j_IR = h.j_IR
            , j_OL = if areOrderedSiblings_Point (Point { path: h.path_O, j: (h.path_I # Ne.head # getIndexesAroundStep)._R }) (Point p') then h.j_OL else h'.j_OL
            }
        )
        focus'
      h' -> Just h'
  -- adjust ZipperH's InnerLeft Point
  InnerLeft_ZipperFocus | isPrefix_Path h.path_O p'.path ->
    drag (Point_Handle hp._OL) (Point p') e >>= case _ of
      ZipperH_Handle (ZipperH h') focus' -> pure $ ZipperH_Handle
        ( ZipperH h'
            { j_OL = h.j_OL
            , j_OR = h.j_OR
            , j_IR = if areOrderedSiblings_Point (Point p') hp._IR then h.j_IR else h'.j_IR
            }
        )
        focus'
      h' -> Just h'
  -- adjust ZipperH's InnerRight Point
  InnerRight_ZipperFocus | isPrefix_Path h.path_O p'.path ->
    drag (Point_Handle hp._OR) (Point p') e >>= case _ of
      ZipperH_Handle (ZipperH h') focus' -> pure $ ZipperH_Handle
        ( ZipperH h'
            { j_OL = h.j_OL
            , j_OR = h.j_OR
            , j_IL = if areOrderedSiblings_Point hp._IL (Point p') then h.j_IL else h'.j_IL
            }
        )
        focus'
      h' -> Just h'
  --
  _ -> Nothing
  where
  hp = ZipperH h # getEndPoints_ZipperH
  h_path_I = ZipperH h # getTotalInnerPath_ZipperH # fromNePath

