module Data.Expr.Move where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Debug as Debug
import Utility (todo)

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
  L | extreme_j._L == p.j, Just { init: path', last: i } <- p.path # List.unsnoc ->
    Just $ Point_Handle $ Point { path: path', j: (i # getIndexesAroundStep)._L }
  L | extreme_j._L < p.j, i <- (getStepsAroundIndex p.j)._L ->
    Just $ Point_Handle $ Point { path: p.path <> (i : Nil), j: (at_parent.at # getKid_Expr i # getExtremeIndexes)._R }
  R | p.j == extreme_j._R, Just { init: path', last: i } <- p.path # List.unsnoc ->
    Just $ Point_Handle $ Point { path: path', j: (i # getIndexesAroundStep)._R }
  R | p.j < extreme_j._R, i <- (getStepsAroundIndex p.j)._R ->
    Just $ Point_Handle $ Point { path: p.path <> (i : Nil), j: (at_parent.at # getKid_Expr i # getExtremeIndexes)._L }
  _ -> Nothing
  where
  at_parent = e # atSubExpr p.path
  extreme_j = at_parent.at # getExtremeIndexes
