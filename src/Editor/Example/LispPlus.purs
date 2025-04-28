module Editor.Example.LispPlus where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.Foldable (and, fold, or)
import Data.List (List(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor (Editor(..), mkPasteFragmentEdit)
import Halogen.HTML as HH
import Options.Applicative.Internal.Utils (startsWith)
import Ui.Event (matchKeyInfo)
import Ui.Halogen (classes)
import Utility (isWhitespaceFree)

data L
  = Root
  | Group
  | Symbol String
  | Integral

instance Show L where
  show Root = "#Root"
  show Group = "#Group"
  show (Symbol s) = s
  show Integral = "#Integral"

derive instance Eq L

derive instance Ord L

validPoint :: Expr L -> Point -> Boolean
validPoint expr (Point p) = e'.l `Set.member` ls
  where
  Expr e' = (expr # atSubExpr p.path).here
  ls = Set.fromFoldable [ Root, Group, Integral ]

editor :: Editor L
editor = Editor
  { name: "Lisp"
  , initial_expr: Root % [ Integral % [ Symbol "x" % [], Symbol "0" % [], Symbol "∞" % [], Symbol "x" % [] ] ]
  , initial_handle: Point_Handle (Point { path: mempty, j: wrap 0 })
  , getEditMenu: \root handle query -> fold
      [ case query of
          "" -> []
          _ | "group" # startsWith (String.Pattern query) ->
            case handle of
              Point_Handle _ ->
                [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Symbol query % [] ]
                , mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Group % [] ]
                ]
              SpanH_Handle _ _ ->
                [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Symbol query % [] ]
                , mkPasteFragmentEdit root handle $ Zipper_Fragment $ Zipper
                    { kids_L: []
                    , inside: SpanContext
                        { _O: ExprContext Nil
                        , _I: SpanTooth { l: Group, kids_L: [], kids_R: [] }
                        }
                    , kids_R: []
                    }
                ]
              ZipperH_Handle _ _ ->
                [ mkPasteFragmentEdit root handle $ Zipper_Fragment $ Zipper
                    { kids_L: []
                    , inside: SpanContext
                        { _O: ExprContext Nil
                        , _I: SpanTooth { l: Group, kids_L: [], kids_R: [] }
                        }
                    , kids_R: []
                    }
                ]
          _ | "integral" # startsWith (String.Pattern query) ->
            case handle of
              Point_Handle _ ->
                [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Symbol query % [] ]
                , mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Integral % [] ]
                ]
              SpanH_Handle _ _ ->
                [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Symbol query % [] ]
                , mkPasteFragmentEdit root handle $ Zipper_Fragment $ Zipper
                    { kids_L: []
                    , inside: SpanContext
                        { _O: ExprContext Nil
                        , _I: SpanTooth { l: Integral, kids_L: [], kids_R: [] }
                        }
                    , kids_R: []
                    }
                ]
              ZipperH_Handle _ _ ->
                [ mkPasteFragmentEdit root handle $ Zipper_Fragment $ Zipper
                    { kids_L: []
                    , inside: SpanContext
                        { _O: ExprContext Nil
                        , _I: SpanTooth { l: Integral, kids_L: [], kids_R: [] }
                        }
                    , kids_R: []
                    }
                ]
          _ | query # isWhitespaceFree ->
            [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Symbol query % [] ] ]
          _ ->
            none
      ]
  , getShortcut: \root handle ki -> case unit of
      _ | ki # matchKeyInfo (_ == "(") { cmd: pure false, alt: pure false } ->
        pure $ mkPasteFragmentEdit root handle $ Zipper_Fragment $ Zipper
          { kids_L: []
          , inside: SpanContext
              { _O: ExprContext Nil
              , _I: SpanTooth { l: Group, kids_L: [], kids_R: [] }
              }
          , kids_R: []
          }
      _ -> none
  , isValidHandle: \expr handle -> case handle of
      Point_Handle p -> and [ validPoint expr p ]
      SpanH_Handle sh _ -> and [ validPoint expr p._L, validPoint expr p._R ]
        where
        p = getEndPoints_SpanH sh
      ZipperH_Handle zh _ -> and [ validPoint expr p._OL, validPoint expr p._IL, validPoint expr p._IR, validPoint expr p._OR ]
        where
        p = getEndPoints_ZipperH zh
  , assembleExpr: \_args@{ label, kids, points } ->
      case label of
        Root -> Array.fold
          [ Array.fold $ Array.zipWith (\kid point -> [ point, kid ]) kids points
          , [ points # Array.last # fromMaybe (HH.div [] [ HH.text "{{missing last point}}" ]) ]
          ]
        Group -> Array.fold
          [ [ HH.div [ classes [ "Punctuation" ] ] [ HH.text "(" ] ]
          , Array.fold $ Array.zipWith (\kid point -> [ point, kid ]) kids points
          , [ points # Array.last # fromMaybe (HH.div [] [ HH.text "{{missing last point}}" ]) ]
          , [ HH.div [ classes [ "Punctuation" ] ] [ HH.text ")" ] ]
          ]
        Symbol str -> Array.fold
          [ [ HH.div [ classes [ "label" ] ] [ HH.text str ] ]
          ]
        Integral -> case kids /\ points of
          [ k0, k1, k2, k3 ] /\ [ p0, p1, p2, p3, p4 ] ->
            [ HH.div [ classes [ "Punctuation" ] ] [ HH.text "(" ]
            , HH.div [ classes [ "Integral-kid-container", "Integral-integrand" ] ] [ HH.div [] [ HH.text "∫" ] ]
            , p0
            , HH.div [ classes [ "Integral-kid-container", "Integral-var" ] ] [ k0 ]
            , HH.div [ classes [ "Integral-kid-container", "Integral-eq" ] ] [ HH.div [] [ HH.text "=" ] ]
            , p1
            , HH.div [ classes [ "Integral-kid-container", "Integral-start" ] ] [ k1 ]
            , p2
            , HH.div [ classes [ "Integral-kid-container", "Integral-end" ] ] [ k2 ]
            , p3
            , HH.div [ classes [ "Integral-kid-container", "Integral-body" ] ] [ k3 ]
            , p4
            , HH.div [ classes [ "Punctuation" ] ] [ HH.text ")" ]
            ]
          _ -> Array.fold
            [ [ HH.div [ classes [ "Punctuation" ] ] [ HH.text "[" ] ]
            , [ HH.div [ classes [ "label" ] ] [ HH.text "Integral" ] ]
            , Array.fold $ Array.zipWith (\kid point -> [ point, kid ]) kids points
            , [ points # Array.last # fromMaybe (HH.div [] [ HH.text "{{missing last point}}" ]) ]
            , [ HH.div [ classes [ "Punctuation" ] ] [ HH.text "]" ] ]
            ]
  }

