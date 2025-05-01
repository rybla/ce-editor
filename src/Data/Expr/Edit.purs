module Data.Expr.Edit where

import Data.Expr
import Prelude

import Control.Monad.Writer (tell)
import Control.Plus (empty)
import Data.Expr.Drag as Expr.Drag
import Data.Expr.Move as Expr.Move
import Data.Lazy as Lazy
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Unfoldable (none)
import Ui.DiagnosticsPanel.Common as Diagnostic
import Utility (fromMaybeM, guardPure)

--------------------------------------------------------------------------------
-- insert
--------------------------------------------------------------------------------

insert :: forall l. Show l => Fragment l -> EditAt l

insert _ { mb_handle: Nothing } = empty

-- to insert s:Span at p:Point, splice s at p.
insert insertion@(Span_Fragment s) { root: e, mb_handle: Just (Point_Handle (Point p)), clipboard } =
  pure $ Edit
    { info: Insert_EditInfo { insertion }
    , output: Lazy.defer \_ -> pure
        { root: unSpanContext at_p.outside s
        , mb_handle: Just $ Point_Handle (Point { path: p.path, j: p.j + offset_Span s })
        , clipboard
        }
    }
  where
  at_p = e # atPoint (Point p)

-- to insert z:Zipper at p:Point, splice z at p and fill the inside of z with
-- the empty Span.
insert insertion@(Zipper_Fragment z) { root: e, mb_handle: Just (Point_Handle (Point p)), clipboard } =
  pure $ Edit
    { info: Insert_EditInfo { insertion }
    , output: Lazy.defer \_ -> pure
        { root: unSpanContext at_p.outside (unZipper z (Span none))
        , mb_handle: Just $ Point_Handle $ Point
            { path: p.path <> (p.j # getStepsAroundIndex)._R : (z # getPath_Zipper)
            , j: offset_innerLeft_Zipper z
            }
        , clipboard
        }
    }
  where
  at_p = e # atPoint (Point p)

-- to insert s:Span at sh:SpanH, replace the span at sh with s.
insert insertion@(Span_Fragment s) { root: e, mb_handle: Just (SpanH_Handle (SpanH sh) sf), clipboard } =
  pure $ Edit
    { info: Insert_EditInfo { insertion }
    , output: Lazy.defer \_ -> pure
        { root: unSpanContext at_sh.outside s
        , mb_handle: Just $ Point_Handle $ Point
            { path: sh.path
            , j: case sf of
                Left_SpanFocus -> sh.j_L
                Right_SpanFocus -> sh.j_L + offset_Span s
            }
        , clipboard
        }
    }
  where
  at_sh = e # atSpan (SpanH sh)

insert insertion@(Zipper_Fragment z) { root: e, mb_handle: Just (SpanH_Handle (SpanH sh) sf), clipboard } =
  pure $ Edit
    { info: Insert_EditInfo { insertion }
    , output: Lazy.defer \_ -> pure
        { root: unSpanContext at_sh.outside (unZipper z at_sh.here)
        , mb_handle: Just $ SpanH_Handle
            ( SpanH
                { path: sh.path <> (sh.j_L # getStepsAroundIndex)._R : (z # getPath_Zipper)
                , j_L: offset_innerLeft_Zipper z
                , j_R: offset_innerLeft_Zipper z + offset_Span at_sh.here
                }
            )
            sf
        , clipboard
        }
    }
  where
  at_sh = e # atSpan (SpanH sh)

-- to insert s:Span at zh:ZipperH, replace outer span of zh with s
insert insertion@(Span_Fragment s) { root: e, mb_handle: Just (ZipperH_Handle (ZipperH zh) zf), clipboard } =
  pure $ Edit
    { info: Insert_EditInfo { insertion }
    , output: Lazy.defer \_ -> pure
        { root: unSpanContext at_zh.outside s
        , mb_handle: Just $ SpanH_Handle
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
        , clipboard
        }
    }
  where
  at_zh = e # atZipper (ZipperH zh)

-- to insert z:Zipper at zh:ZipperH, replace the zipper at zh with z.
insert insertion@(Zipper_Fragment z) { root: e, mb_handle: Just (ZipperH_Handle (ZipperH zh) zf), clipboard } =
  pure $ Edit
    { info: Insert_EditInfo { insertion }
    , output: Lazy.defer \_ -> pure
        { root: unSpanContext at_zh.outside (unZipper z at_zh.inside)
        , mb_handle: Just $ SpanH_Handle
            ( let
                outer = Lazy.defer \_ -> SpanH
                  { path: zh.path_O
                  , j_L: zh.j_OL
                  , j_R: zh.j_OL + (offset_outer_Zipper z)._L + Index 1 + (offset_outer_Zipper z)._R
                  }
                inner = Lazy.defer \_ -> SpanH
                  { path: ZipperH zh # getTotalInnerPath_ZipperH # fromNePath
                  , j_L: offset_innerLeft_Zipper z
                  , j_R: offset_innerLeft_Zipper z + (zh.j_IR - zh.j_IL)
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
        , clipboard
        }
    }
  where
  at_zh = e # atZipper (ZipperH zh)

--------------------------------------------------------------------------------
-- paste
--------------------------------------------------------------------------------

paste :: forall l. Show l => EditAt l
paste state = do
  frag <- state.clipboard # fromMaybeM do
    tell [ Diagnostic.text "warning" "can't paste since clipboard is empty" ]
    empty
  insert frag state

--------------------------------------------------------------------------------
-- copy
--------------------------------------------------------------------------------

copy :: forall l. Show l => EditAt l

copy state@{ mb_handle: Just (Point_Handle _) } =
  pure $ Edit
    { info: Copy_EditInfo {}
    , output: Lazy.defer \_ -> do
        pure state { clipboard = pure $ Span_Fragment $ Span none }
    }

copy state@{ root: e, mb_handle: Just (SpanH_Handle (SpanH sh) _sf) } =
  pure $ Edit
    { info: Copy_EditInfo {}
    , output: Lazy.defer \_ -> do
        let at_sh = e # atSpan (SpanH sh)
        pure state { clipboard = pure $ Span_Fragment at_sh.here }
    }

copy state@{ root: e, mb_handle: Just (ZipperH_Handle (ZipperH zh) _zf) } =
  pure $ Edit
    { info: Copy_EditInfo {}
    , output: Lazy.defer \_ -> do
        let at_zh = e # atZipper (ZipperH zh)
        pure state { clipboard = pure $ Zipper_Fragment at_zh.here }
    }

copy { mb_handle: Nothing } = empty

--------------------------------------------------------------------------------
-- delete
--
-- same as cut except preserves clipboard
--------------------------------------------------------------------------------

delete :: forall l. Show l => EditAt l
delete state = do
  Edit edit <- cut state
  pure $ Edit edit { output = edit.output # map (map _ { clipboard = state.clipboard }) }

--------------------------------------------------------------------------------
-- delete'
--
-- same as delete except when handle is a Point, first drags back one movement
-- before deleting.
--------------------------------------------------------------------------------

delete' :: forall l. Show l => { isValidHandle :: Expr l -> Handle -> Boolean } -> EditAt l
delete' { isValidHandle } state@{ root: e, mb_handle: Just (Point_Handle p0) } = do
  let
    mb_handle' = Expr.Move.movePointUntil state.root Expr.Move.L p0 \p ->
      e # Expr.Drag.drag (Point_Handle p0) p >>= guardPure (isValidHandle e)
  case mb_handle' of
    Nothing -> empty
    Just handle' -> delete state { mb_handle = Just handle' }
delete' _ state = delete state

delete'_sibling :: forall l. Show l => { isValidHandle :: Expr l -> Handle -> Boolean } -> EditAt l
delete'_sibling { isValidHandle } state@{ root: e, mb_handle: Just (Point_Handle p0) } = do
  let
    mb_handle' = Expr.Move.movePointUntil state.root Expr.Move.L_sibling p0 \p ->
      e # Expr.Drag.drag (Point_Handle p0) p >>= guardPure (isValidHandle e)
  case mb_handle' of
    Nothing -> empty
    Just handle' -> delete state { mb_handle = Just handle' }
delete'_sibling _ state = delete state

--------------------------------------------------------------------------------
-- cut
-- law: cut = copy >>> delete
--------------------------------------------------------------------------------

cut :: forall l. Show l => EditAt l

cut { root: e, mb_handle: Just (Point_Handle p) } =
  pure $ Edit
    { info: Remove_EditInfo {}
    , output: Lazy.defer \_ -> pure
        { root: e
        , mb_handle: Just $ Point_Handle p
        , clipboard: pure $ Span_Fragment $ Span none
        }
    }

cut { root: e, mb_handle: Just (SpanH_Handle (SpanH sh) _sf) } =
  pure $ Edit
    { info: Remove_EditInfo {}
    , output: Lazy.defer \_ -> do
        let at_sh = e # atSpan (SpanH sh)
        pure
          { root: unSpanContext at_sh.outside (Span none)
          , mb_handle: Just $ Point_Handle (SpanH sh # getEndPoints_SpanH)._L
          , clipboard: pure $ Span_Fragment at_sh.here
          }
    }

cut { root: e, mb_handle: Just (ZipperH_Handle (ZipperH zh) zf) } =
  pure $ Edit
    { info: Remove_EditInfo {}
    , output: Lazy.defer \_ -> do
        let at_zh = e # atZipper (ZipperH zh)
        pure
          { root: unSpanContext at_zh.outside at_zh.inside
          , mb_handle: Just
              if zh.j_IL == zh.j_IR then
                Point_Handle
                  ( Point
                      { path: zh.path_O
                      , j: zh.j_OL
                      }
                  )
              else
                SpanH_Handle
                  ( SpanH
                      { path: zh.path_O
                      , j_L: zh.j_OL
                      , j_R: zh.j_OL + (at_zh.inside # offset_Span)
                      }
                  )
                  ( case zf of
                      OuterLeft_ZipperFocus -> Left_SpanFocus
                      InnerLeft_ZipperFocus -> Left_SpanFocus
                      InnerRight_ZipperFocus -> Right_SpanFocus
                      OuterRight_ZipperFocus -> Right_SpanFocus
                  )
          , clipboard: pure $ Zipper_Fragment at_zh.here
          }
    }

cut { mb_handle: Nothing } = empty
