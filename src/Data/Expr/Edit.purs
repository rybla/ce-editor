module Data.Expr.Edit where

import Data.Expr
import Prelude

import Data.Lazy as Lazy
import Data.List ((:))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)

--------------------------------------------------------------------------------
-- paste
--------------------------------------------------------------------------------

paste :: forall l. Show l => Fragment l -> Handle -> Expr l -> Expr l /\ Handle

-- to paste s:Span at p:Point, splice s at p.
paste (Span_Fragment s) (Point_Handle (Point p)) e =
  unSpanContext at_p.outside s /\
    Point_Handle (Point { path: p.path, j: p.j + offset_Span s })
  where
  at_p = e # atPoint (Point p)

-- to paste z:Zipper at p:Point, splice z at p and fill the inside of z with
-- the empty Span.
paste (Zipper_Fragment z) (Point_Handle (Point p)) e =
  unSpanContext at_p.outside (unZipper z (Span none)) /\
    Point_Handle
      ( Point
          { path: p.path <> (p.j # getStepsAroundIndex)._R : (z # getPath_Zipper)
          , j: offset_inner_Zipper z
          }
      )
  where
  at_p = e # atPoint (Point p)

-- to paste s:Span at sh:SpanH, replace the span at sh with s.
paste (Span_Fragment s) (SpanH_Handle (SpanH sh) sf) e =
  unSpanContext at_sh.outside s /\
    Point_Handle
      ( Point
          { path: sh.path
          , j: case sf of
              Left_SpanFocus -> sh.j_L
              Right_SpanFocus -> sh.j_L + offset_Span s
          }
      )
  where
  at_sh = e # atSpan (SpanH sh)

paste (Zipper_Fragment z) (SpanH_Handle (SpanH sh) sf) e =
  unSpanContext at_sh.outside (unZipper z at_sh.here) /\
    Point_Handle
      ( Point
          { path: sh.path <> (sh.j_L # getStepsAroundIndex)._R : (z # getPath_Zipper)
          , j: case sf of
              Left_SpanFocus -> offset_inner_Zipper z
              Right_SpanFocus -> offset_inner_Zipper z + offset_Span at_sh.here
          }
      )
  where
  at_sh = e # atSpan (SpanH sh)

-- to paste s:Span at zh:ZipperH, replace outer span of zh with s
paste (Span_Fragment s) (ZipperH_Handle (ZipperH zh) zf) e =
  unSpanContext at_zh.outside s /\
    SpanH_Handle
      ( SpanH
          { path: zh.path_O
          , j_L: zh.j_OL
          , j_R: zh.j_OL + offset_Span s
          }
      )
      ( case zf of
          OuterLeft_ZipperFocus -> Left_SpanFocus
          InnerLeft_ZipperFocus -> Left_SpanFocus
          InnerRight_ZipperFocus -> Right_SpanFocus
          OuterRight_ZipperFocus -> Right_SpanFocus
      )
  where
  at_zh = e # atZipper (ZipperH zh)

-- to paste z:Zipper at zh:ZipperH, replace the zipper at zh with z.
paste (Zipper_Fragment z) (ZipperH_Handle (ZipperH zh) zf) e =
  unSpanContext at_zh.outside (unZipper z at_zh.inside) /\
    SpanH_Handle
      ( let
          outer = Lazy.defer \_ -> SpanH
            { path: zh.path_O
            , j_L: zh.j_OL
            , j_R: zh.j_OL + (offset_outer_Zipper z)._L + (offset_outer_Zipper z)._R -- TODO: +1 here?
            }
          inner = Lazy.defer \_ -> SpanH
            { path: ZipperH zh # getTotalInnerPath_ZipperH # fromNePath
            , j_L: zh.j_IL
            , j_R: zh.j_IL + offset_inner_Zipper z
            }
        in
          case zf of
            OuterLeft_ZipperFocus -> Lazy.force outer
            InnerLeft_ZipperFocus -> Lazy.force inner
            InnerRight_ZipperFocus -> Lazy.force inner
            OuterRight_ZipperFocus -> Lazy.force outer
      )
      ( case zf of
          OuterLeft_ZipperFocus -> Left_SpanFocus
          InnerLeft_ZipperFocus -> Left_SpanFocus
          InnerRight_ZipperFocus -> Right_SpanFocus
          OuterRight_ZipperFocus -> Right_SpanFocus
      )
  where
  at_zh = e # atZipper (ZipperH zh)

--------------------------------------------------------------------------------
-- cut
--------------------------------------------------------------------------------

cut :: forall l. Show l => Handle -> Expr l -> Expr l /\ Handle /\ Fragment l
cut (Point_Handle p) e = e /\ Point_Handle p /\ Span_Fragment (Span none)
cut (SpanH_Handle (SpanH sh) _sf) e =
  unSpanContext at_sh.outside (Span none)
    /\ Point_Handle (SpanH sh # getEndPoints_SpanH)._L
    /\ Span_Fragment at_sh.here
  where
  at_sh = e # atSpan (SpanH sh)
cut (ZipperH_Handle (ZipperH zh) zf) e =
  unSpanContext at_zh.outside at_zh.inside
    /\ SpanH_Handle
      ( SpanH
          { path: zh.path_O
          , j_L: zh.j_OL
          , j_R: zh.j_OR
          }
      )
      ( case zf of
          OuterLeft_ZipperFocus -> Left_SpanFocus
          InnerLeft_ZipperFocus -> Left_SpanFocus
          InnerRight_ZipperFocus -> Right_SpanFocus
          OuterRight_ZipperFocus -> Right_SpanFocus
      )
    /\ Zipper_Fragment at_zh.here
  where
  at_zh = e # atZipper (ZipperH zh)

