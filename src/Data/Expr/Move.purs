module Data.Expr.Move where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Expr (Expr, Handle(..), Index(..), Point(..), SpanFocus(..), ZipperFocus(..), atSubExpr, getExtremeIndexes, getFocusPoint, getIndexesAroundStep, getInnerSpanH_ZipperH, getKid_Expr, getOuterSpanH_ZipperH, getStepsAroundIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Unfoldable (none)
import Ui.Event (KeyInfo(..))

data Dir = L | R | L_sibling | R_sibling

derive instance Generic Dir _

instance Show Dir where
  show x = genericShow x

instance Eq Dir where
  eq x = genericEq x

fromKeyInfoToMoveDir :: KeyInfo -> Maybe Dir
-- ArrowRight
fromKeyInfoToMoveDir (KeyInfo { key: "ArrowRight", shift: false, alt: false, cmd: false }) = Just R
fromKeyInfoToMoveDir (KeyInfo { key: "ArrowRight", shift: false, alt: true, cmd: false }) = Just R_sibling
-- ArrowLeft
fromKeyInfoToMoveDir (KeyInfo { key: "ArrowLeft", shift: false, alt: false, cmd: false }) = Just L
fromKeyInfoToMoveDir (KeyInfo { key: "ArrowLeft", shift: false, alt: true, cmd: false }) = Just L_sibling
-- Space
fromKeyInfoToMoveDir (KeyInfo { key: " ", shift: false, alt: false, cmd: false }) = Just R
fromKeyInfoToMoveDir (KeyInfo { key: " ", shift: false, alt: true, cmd: false }) = Just R_sibling
-- Shift+Space
fromKeyInfoToMoveDir (KeyInfo { key: " ", shift: true, alt: false, cmd: false }) = Just L
fromKeyInfoToMoveDir (KeyInfo { key: " ", shift: true, alt: true, cmd: false }) = Just L_sibling
-- Tab
fromKeyInfoToMoveDir (KeyInfo { key: "Tab", shift: false, alt: false, cmd: false }) = Just R
fromKeyInfoToMoveDir (KeyInfo { key: "Tab", shift: false, alt: true, cmd: false }) = Just R_sibling
-- Shift+Tab
fromKeyInfoToMoveDir (KeyInfo { key: "Tab", shift: true, alt: false, cmd: false }) = Just L
fromKeyInfoToMoveDir (KeyInfo { key: "Tab", shift: true, alt: true, cmd: false }) = Just L_sibling
-- 
fromKeyInfoToMoveDir _ = Nothing

fromKeyInfoToDragMoveDir :: KeyInfo -> Maybe Dir
fromKeyInfoToDragMoveDir (KeyInfo { key: "ArrowLeft", shift: true, alt: false, cmd: false }) = Just L
fromKeyInfoToDragMoveDir (KeyInfo { key: "ArrowLeft", shift: true, alt: true, cmd: false }) = Just L_sibling
fromKeyInfoToDragMoveDir (KeyInfo { key: "ArrowRight", shift: true, alt: false, cmd: false }) = Just R
fromKeyInfoToDragMoveDir (KeyInfo { key: "ArrowRight", shift: true, alt: true, cmd: false }) = Just R_sibling
fromKeyInfoToDragMoveDir (KeyInfo { key: " ", shift: true, alt: false, cmd: false }) = Just R
fromKeyInfoToDragMoveDir (KeyInfo { key: " ", shift: true, alt: true, cmd: false }) = Just R_sibling
fromKeyInfoToDragMoveDir _ = Nothing

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

movePoint :: forall l. Show l => Expr l -> Dir -> Point -> Maybe Point
movePoint e dir (Point p) = case dir of
  -- 
  L | extreme_j._L == p.j, Just { init: path', last: i } <- p.path # List.unsnoc -> Just $ Point { path: path', j: (i # getIndexesAroundStep)._L }
  L_sibling | extreme_j._L == p.j, Just { init: path', last: i } <- p.path # List.unsnoc -> Just $ Point { path: path', j: (i # getIndexesAroundStep)._L }
  L | extreme_j._L < p.j, i <- (getStepsAroundIndex p.j)._L, Just kid <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path <> (i : Nil), j: (kid # getExtremeIndexes)._R }
  L | extreme_j._L < p.j, i <- (getStepsAroundIndex p.j)._L, Nothing <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path, j: p.j - Index 1 }
  L_sibling | extreme_j._L < p.j, i <- (getStepsAroundIndex p.j)._L -> Just $ Point { path: p.path, j: p.j - Index 1 }
  --
  R | p.j == extreme_j._R, Just { init: path', last: i } <- p.path # List.unsnoc -> Just $ Point { path: path', j: (i # getIndexesAroundStep)._R }
  R_sibling | p.j == extreme_j._R, Just { init: path', last: i } <- p.path # List.unsnoc -> Just $ Point { path: path', j: (i # getIndexesAroundStep)._R }
  R | p.j < extreme_j._R, i <- (getStepsAroundIndex p.j)._R, Nothing <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path, j: p.j + Index 1 }
  R | p.j < extreme_j._R, i <- (getStepsAroundIndex p.j)._R, Just kid <- at_e.here # getKid_Expr i -> Just $ Point { path: p.path <> (i : Nil), j: (kid # getExtremeIndexes)._L }
  R_sibling | p.j < extreme_j._R, i <- (getStepsAroundIndex p.j)._R -> Just $ Point { path: p.path, j: p.j + Index 1 }
  -- 
  _ -> Nothing
  where
  at_e = e # atSubExpr p.path
  extreme_j = at_e.here # getExtremeIndexes

data Cycle = N | P

derive instance Generic Cycle _

instance Show Cycle where
  show x = genericShow x

instance Eq Cycle where
  eq x = genericEq x

fromKeyInfoToCycle :: KeyInfo -> Maybe Cycle
fromKeyInfoToCycle (KeyInfo { key: "ArrowLeft", shift: false, alt: false, cmd: true }) = Just P
fromKeyInfoToCycle (KeyInfo { key: "ArrowRight", shift: false, alt: false, cmd: true }) = Just N
fromKeyInfoToCycle _ = Nothing

cycleHandleFocus :: Cycle -> Handle -> Handle
cycleHandleFocus _c (Point_Handle p) = Point_Handle p
cycleHandleFocus c (SpanH_Handle h f) = SpanH_Handle h (f # cycleSpanFocus c)
cycleHandleFocus c (ZipperH_Handle h f) = ZipperH_Handle h (f # cycleZipperFocus c)

cycleSpanFocus :: Cycle -> SpanFocus -> SpanFocus
cycleSpanFocus _c Left_SpanFocus = Right_SpanFocus
cycleSpanFocus _c Right_SpanFocus = Left_SpanFocus

cycleZipperFocus :: Cycle -> ZipperFocus -> ZipperFocus
cycleZipperFocus P OuterLeft_ZipperFocus = OuterRight_ZipperFocus
cycleZipperFocus P OuterRight_ZipperFocus = InnerRight_ZipperFocus
cycleZipperFocus P InnerRight_ZipperFocus = InnerLeft_ZipperFocus
cycleZipperFocus P InnerLeft_ZipperFocus = OuterLeft_ZipperFocus
cycleZipperFocus N OuterLeft_ZipperFocus = InnerLeft_ZipperFocus
cycleZipperFocus N InnerLeft_ZipperFocus = InnerRight_ZipperFocus
cycleZipperFocus N InnerRight_ZipperFocus = OuterRight_ZipperFocus
cycleZipperFocus N OuterRight_ZipperFocus = OuterLeft_ZipperFocus

escape :: Handle -> Maybe Handle
escape (Point_Handle _) = none
escape h@(SpanH_Handle _ _) = pure $ Point_Handle $ getFocusPoint h
escape (ZipperH_Handle h OuterLeft_ZipperFocus) = pure $ SpanH_Handle (h # getOuterSpanH_ZipperH) Right_SpanFocus
escape (ZipperH_Handle h InnerLeft_ZipperFocus) = pure $ SpanH_Handle (h # getInnerSpanH_ZipperH) Right_SpanFocus
escape (ZipperH_Handle h InnerRight_ZipperFocus) = pure $ SpanH_Handle (h # getInnerSpanH_ZipperH) Left_SpanFocus
escape (ZipperH_Handle h OuterRight_ZipperFocus) = pure $ SpanH_Handle (h # getOuterSpanH_ZipperH) Left_SpanFocus

