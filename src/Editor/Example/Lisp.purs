module Editor.Example.Lisp where

import Data.Expr
import Prelude

import Data.Either (Either(..))
import Data.Foldable (and, fold, or)
import Data.List (List(..))
import Data.Newtype (wrap)
import Data.String as String
import Data.Unfoldable (none)
import Editor (Editor(..), mkPasteFragmentEdit)
import Editor.Notation as Notation
import Halogen.HTML as HH
import Options.Applicative.Internal.Utils (startsWith)
import Ui.Event (matchKeyInfo)
import Ui.Halogen (classes)
import Utility (isWhitespaceFree)

data L
  = Root
  | Group
  | Integral
  | Arg
  | Symbol String

derive instance Eq L

instance Show L where
  show Root = "#Root"
  show Integral = "#Integral"
  show Arg = "#Arg"
  show Group = "#Group"
  show (Symbol s) = s

isValidPoint :: Expr L -> Point -> Boolean
isValidPoint expr (Point p) = or
  [ e'.l == Root
  , e'.l == Group
  , e'.l == Arg
  ]
  where
  Expr e' = (expr # atSubExpr p.path).here

editor :: Editor L
editor = Editor
  { name: "Lisp"
  , initial_expr: Root % []
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
          _ | query # startsWith (String.Pattern "integral") ->
            [ mkPasteFragmentEdit root handle $ Span_Fragment $ Span [ Integral % [ Arg % [], Arg % [], Arg % [], Arg % [] ] ] ]
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
      Point_Handle p -> and [ isValidPoint expr p ]
      SpanH_Handle sh _ -> and [ isValidPoint expr p._L, isValidPoint expr p._R ]
        where
        p = getEndPoints_SpanH sh
      ZipperH_Handle zh _ -> and [ isValidPoint expr p._OL, isValidPoint expr p._IL, isValidPoint expr p._IR, isValidPoint expr p._OR ]
        where
        p = getEndPoints_ZipperH zh
  , assembleExpr:
      let
        root = Notation.parseString "Root [ \n \t * \n ]"
        group = Notation.parseString "( \n \t * \n )"
        integral = Notation.parseString "(∫ _ from _ to _ of _ )"
        arg = Notation.parseString "*"
      in
        Notation.mkAssembleExpr case _ of
          Root -> root
          Group -> group
          Integral -> integral
          Arg -> arg
          Symbol s -> [ Notation.Punc [ HH.div [ classes [ "Punctuation" ] ] [ HH.text s ] ] ]
  }

