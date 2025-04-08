module Data.Expr.Move where

import Data.Expr
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data Dir = L | R

derive instance Generic Dir _

instance Show Dir where
  show x = genericShow x

instance Eq Dir where
  eq x = genericEq x

move :: Expr -> Dir -> Handle -> Maybe Handle
move e dir (Point_Handle p) = move_Point e dir p
move _ _ _ = Nothing

move_Point :: Expr -> Dir -> Point -> Maybe Handle
move_Point e dir (Point p) = case dir of
  L | i._L < p.j, i_L <- (getStepsAroundIndex p.j)._L ->
    case at_parent.at # getKid_Expr i_L # getBottomRightPoint of
      Nothing -> Just $ Point_Handle $ Point p { j = p.j - Index 1 }
      Just (Point p') -> Just $ Point_Handle $ Point p' { path = p.path <> i_L : p'.path }
  R | p.j < i._R, i_R <- (getStepsAroundIndex p.j)._R ->
    case at_parent.at # getKid_Expr i_R # getBottomLeftPoint of
      Nothing -> Just $ Point_Handle $ Point p { j = p.j + Index 1 }
      Nothing -> Nothing
      Just (Point p') -> Just $ Point_Handle $ Point p' { path = p.path <> i_R : p'.path }
  _ -> Nothing
  where
  at_parent = e # atSubExpr p.path
  i = at_parent.at # getExtremeIndexes

getBottomRightPoint :: Expr -> Maybe Point
getBottomRightPoint (Expr e0) = do
  { _R: j_R } <- Expr e0 # getExtremeSteps
  Just $ go Nil j_R (Expr e0 # getKid_Expr j_R)
  where
  go :: Path -> Step -> Expr -> Point
  go rev_path i (Expr e) = case Expr e # getExtremeSteps of
    Nothing -> Point { path: rev_path # List.reverse, j: (i # getIndexesAroundStep)._R }
    Just { _R: j_R } -> go (i : rev_path) j_R (Expr e # getKid_Expr j_R)

getBottomLeftPoint :: Expr -> Maybe Point
getBottomLeftPoint (Expr e0) = do
  { _L: j_L } <- Expr e0 # getExtremeSteps
  Just $ go Nil j_L (Expr e0 # getKid_Expr j_L)
  where
  go :: Path -> Step -> Expr -> Point
  go rev_path i (Expr e) = case Expr e # getExtremeSteps of
    Nothing -> Point { path: rev_path # List.reverse, j: (i # getIndexesAroundStep)._L }
    Just { _L: j_L } -> go (i : rev_path) j_L (Expr e # getKid_Expr j_L)

