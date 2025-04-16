module Data.Expr.Move where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Debug as Debug
import Utility (todo)

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

moveUntil :: forall l a. Show l => Expr l -> Dir -> Handle -> (Point -> Maybe a) -> Maybe a
moveUntil e dir h f = case move e dir h of
  Nothing -> Nothing
  Just p -> go p
  where
  go p = case f p of
    Nothing -> case move e dir (Point_Handle p) of
      Nothing -> Nothing
      Just p' -> go p'
    Just a -> pure a

move :: forall l. Show l => Expr l -> Dir -> Handle -> Maybe Point
move e dir h = h # getFocusPoint # move_Point e dir

-- BUG: during dragging, something messes up here when I move from down-right and it goes down-left instead???
move_Point :: forall l. Show l => Expr l -> Dir -> Point -> Maybe Point
move_Point e dir (Point p) = case dir of
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

