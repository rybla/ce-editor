module Editor.Example.Lisp where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (and, fold, length)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String as String
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor (Editor(..), mkPasteFragmentEdit)
import Editor.Common (renderWarning)
import Editor.Notation as Notation
import Halogen.HTML as HH
import Options.Applicative.Internal.Utils (startsWith)
import Ui.Event (matchKeyInfo)
import Ui.Halogen (classes)
import Utility (isWhitespaceFree)

editor :: Editor L
editor = Editor
  { name: "Lisp"
  , initial_expr: "Root" % []
  , initial_handle: Point_Handle (Point { path: mempty, j: wrap 0 })
  , getEditMenu:
      \root handle query -> fold
        [ case query of
            "" -> []
            _ | "group" # startsWith (String.Pattern query) -> case handle of
              Point_Handle _ ->
                [ pasteGroup_Span root handle
                , pasteLiteral root handle query
                ]
              SpanH_Handle _ _ ->
                [ pasteGroup_Zipper root handle
                , pasteLiteral root handle query
                ]
              ZipperH_Handle _ _ ->
                [ pasteGroup_Zipper root handle
                ]
            _ | "integral" # startsWith (String.Pattern query) -> case handle of
              Point_Handle _ ->
                [ pasteIntegral_Zipper root handle
                , pasteLiteral root handle query
                ]
              SpanH_Handle _ _ ->
                [ pasteIntegral_Zipper root handle
                , pasteLiteral root handle query
                ]
              ZipperH_Handle _ _ ->
                [ pasteIntegral_Zipper root handle
                ]
            _ | "linebreak" # startsWith (String.Pattern query) -> case handle of
              Point_Handle _ ->
                [ pasteLineBreak_Span root handle
                , pasteLiteral root handle query
                ]
              SpanH_Handle _ _ ->
                [ pasteLineBreak_Span root handle
                , pasteLiteral root handle query
                ]
              ZipperH_Handle _ _ ->
                []
              _ -> []
            _ | query # isWhitespaceFree -> case handle of
              Point_Handle _ -> [ pasteLiteral root handle query ]
              SpanH_Handle _ _ -> [ pasteLiteral root handle query ]
              ZipperH_Handle _ _ -> []
            _ ->
              none
        ]
  , getShortcut: \root handle ki -> case unit of
      _ | ki # matchKeyInfo (_ == "(") { cmd: pure false, alt: pure false } ->
        pure $ pasteGroup_Zipper root handle
      _ | ki # matchKeyInfo (_ == "Enter") { cmd: pure false, alt: pure false } ->
        pure $ pasteLineBreak_Span root handle
      _ -> none
  , isValidHandle: \expr handle -> case handle of
      Point_Handle p -> and [ isValidPoint expr p ]
      SpanH_Handle sh _ -> and [ isValidPoint expr p._L, isValidPoint expr p._R ]
        where
        p = getEndPoints_SpanH sh
      ZipperH_Handle zh _ -> and [ isValidPoint expr p._OL, isValidPoint expr p._IL, isValidPoint expr p._IR, isValidPoint expr p._OR ]
        where
        p = getEndPoints_ZipperH zh
  , assembleExpr:
      let
        root = Notation.parseString "*"
        group = Notation.parseString "'(' * ')'"
        integral = Notation.parseString "'(' ∫ _ from _ to _ of _ ')'"
        linebreak = Notation.parseString "\n"
      in
        Notation.mkAssembleExpr case _ of
          { label: "Root" } -> Left root
          { label: "Group" } -> Left group
          { label: "LineBreak" } -> Left linebreak
          { label: "Integral" } -> Left integral
          args@{ label: "Arg" }
            | length args.kids == 0 -> Right $ pure $
                [ HH.div [ classes [ "Token", "before-empty-Arg-kids" ] ] []
                , args.points # Array.last # fromMaybe (renderWarning $ "missing point #" <> show @Int (length args.points))
                ]
            | otherwise -> Right do
                kids <- args.kids # traverseWithIndex \i -> map (i /\ _)
                pure $ fold $
                  [ fold $ Array.zipWith
                      ( \point (i /\ kid) -> fold
                          [ [ point ]
                          , if i == 0 then [] else [ HH.div [ classes [ "Token", "punctuation", "before-extra-Arg-kid" ] ] [ HH.text "[" ] ]
                          , kid
                          , if i == 0 then [] else [ HH.div [ classes [ "Token", "punctuation", "after-extra-Arg-kid" ] ] [ HH.text "]" ] ]
                          ]
                      )
                      args.points
                      kids
                  , [ args.points # Array.last # fromMaybe do renderWarning $ "missing point #" <> show @Int (length args.points) ]
                  ]
          { label } -> Left [ Notation.Punc do pure [ HH.div [ classes [ "Token", "punctuation", "keyword" ] ] [ HH.text label ] ] ]
  }

type L = String

isValidPoint :: Expr L -> Point -> Boolean
isValidPoint expr (Point p) = e'.l `Set.member` ls
  where
  Expr e' = (expr # atSubExpr p.path).here
  ls = Set.fromFoldable [ "Root", "Group", "Arg" ]

pasteLiteral root handle query = mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ query % [] ]

pasteGroup_Span root handle = mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ "Group" % [] ]
pasteGroup_Zipper root handle = mkPasteFragmentEdit root handle $ Zipper_Fragment $ Zipper
  { kids_L: []
  , inside: SpanContext
      { _O: ExprContext Nil
      , _I: SpanTooth { l: "Group", kids_L: [], kids_R: [] }
      }
  , kids_R: []
  }

pasteIntegral_Span root handle = mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ "Integral" % [ "Arg" % [], "Arg" % [], "Arg" % [], "Arg" % [] ] ]

pasteIntegral_Zipper root handle = mkPasteFragmentEdit root handle $ Zipper_Fragment $ Zipper
  { kids_L: []
  , inside: SpanContext
      { _O: ExprContext $ Tooth { l: "Integral", kids_L: [ "Arg" % [], "Arg" % [], "Arg" % [] ], kids_R: [] } : Nil
      , _I: SpanTooth { l: "Arg", kids_L: [], kids_R: [] }
      }
  , kids_R: []
  }

pasteLineBreak_Span root handle = mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ "LineBreak" % [] ]

