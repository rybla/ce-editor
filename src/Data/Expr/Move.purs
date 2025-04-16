module Data.Expr.Move where

import Data.Expr
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)

data Dir = L | R

derive instance Generic Dir _

instance Show Dir where
  show x = genericShow x

instance Eq Dir where
  eq x = genericEq x

fromKeyToDir :: String -> Maybe Dir
fromKeyToDir "ArrowLeft" = Just L
fromKeyToDir "ArrowRight" = Just R
fromKeyToDir _ = Nothing

movePointUntil :: forall l a. Show l => Expr l -> Dir -> Point -> (Point -> Maybe a) -> Maybe a
movePointUntil e dir h f = case movePoint e dir h of
  Nothing -> Nothing
  Just p -> go p
  where
  go p = case f p of
    Nothing -> case movePoint e dir p of
      Nothing -> Nothing
      Just p' -> go p'
    Just a -> pure a

-- BUG: during dragging, something messes up here when I move from down-right and it goes down-left instead???
movePoint :: forall l. Show l => Expr l -> Dir -> Point -> Maybe Point
movePoint e dir (Point p) = case dir of
  L | extreme_j._L == p.j, Just { init: path', last: i } <- p.path # List.unsnoc -> Just $ Point { path: path', j: (i # getIndexesAroundStep)._L }
  L | extreme_j._L < p.j, i <- (getStepsAroundIndex p.j)._L, Just kid <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path <> (i : Nil), j: (kid # getExtremeIndexes)._R }
  L | extreme_j._L < p.j, i <- (getStepsAroundIndex p.j)._L, Nothing <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path, j: p.j - wrap 1 }
  R | p.j == extreme_j._R, Just { init: path', last: i } <- p.path # List.unsnoc -> Just $ Point { path: path', j: (i # getIndexesAroundStep)._R }
  R | p.j < extreme_j._R, i <- (getStepsAroundIndex p.j)._R, Nothing <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path, j: p.j + wrap 1 }
  R | p.j < extreme_j._R, i <- (getStepsAroundIndex p.j)._R, Just kid <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path <> (i : Nil), j: (kid # getExtremeIndexes)._L }
  _ -> Nothing
  where
  at_e = e # atSubExpr p.path
  extreme_j = at_e.here # getExtremeIndexes

moveHandleFocus :: Dir -> Handle -> Handle
moveHandleFocus _dir (Point_Handle p) = Point_Handle p
moveHandleFocus dir (SpanH_Handle h f) = SpanH_Handle h (f # moveSpanFocus dir)
moveHandleFocus dir (ZipperH_Handle h f) = ZipperH_Handle h (f # moveZipperFocus dir)

moveSpanFocus :: Dir -> SpanFocus -> SpanFocus
moveSpanFocus _dir Left_SpanFocus = Right_SpanFocus
moveSpanFocus _dir Right_SpanFocus = Left_SpanFocus

moveZipperFocus :: Dir -> ZipperFocus -> ZipperFocus
moveZipperFocus L OuterLeft_ZipperFocus = OuterRight_ZipperFocus
moveZipperFocus L OuterRight_ZipperFocus = InnerRight_ZipperFocus
moveZipperFocus L InnerRight_ZipperFocus = InnerLeft_ZipperFocus
moveZipperFocus L InnerLeft_ZipperFocus = OuterLeft_ZipperFocus
moveZipperFocus R OuterLeft_ZipperFocus = InnerLeft_ZipperFocus
moveZipperFocus R InnerLeft_ZipperFocus = InnerRight_ZipperFocus
moveZipperFocus R InnerRight_ZipperFocus = OuterRight_ZipperFocus
moveZipperFocus R OuterRight_ZipperFocus = OuterLeft_ZipperFocus

