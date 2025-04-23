module Editor.Example.Lisp where

import Data.Expr
import Prelude

import Data.Array as Array
import Data.Foldable (and, fold, or)
import Data.List (List(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap)
import Data.String as String
import Editor (Editor(..))
import Halogen.HTML as HH
import Options.Applicative.Internal.Utils (startsWith)
import Ui.Halogen (classes)

data L
  = Root
  | Group
  | Symbol String

derive instance Eq L

instance Show L where
  show Root = "#Root"
  show Group = "#Group"
  show (Symbol s) = s

-- (expr # atSubExpr p.path).here # \(Expr e) -> e.l == Group
validPoint :: Expr L -> Point -> Boolean
validPoint expr (Point p) = or
  [ e'.l == Root
  , e'.l == Group
  ]
  where
  Expr e' = (expr # atSubExpr p.path).here

editor :: Editor L
editor = Editor
  { name: "Lisp"
  , initial_expr: Root % []
  , initial_handle: Point_Handle (Point { path: mempty, j: wrap 0 })
  , bufferOptions: \expr handle query -> fold
      [ case query of
          "" -> []
          _ | "group" # startsWith (String.Pattern query) ->
            case handle of
              Point_Handle _ -> [ Fragment_BufferOption $ Span_Fragment $ Span [ Group % [] ] ]
              SpanH_Handle _ _ ->
                [ Fragment_BufferOption $ Zipper_Fragment $ Zipper
                    { kids_L: []
                    , inside: SpanContext
                        { _O: ExprContext Nil
                        , _I: SpanTooth { l: Group, kids_L: [], kids_R: [] }
                        }
                    , kids_R: []
                    }
                ]
              ZipperH_Handle _ _ ->
                [ Fragment_BufferOption $ Zipper_Fragment $ Zipper
                    { kids_L: []
                    , inside: SpanContext
                        { _O: ExprContext Nil
                        , _I: SpanTooth { l: Group, kids_L: [], kids_R: [] }
                        }
                    , kids_R: []
                    }
                ]
          _ -> [ Fragment_BufferOption $ Span_Fragment $ Span [ Symbol query % [] ] ]
      ]
  , validHandle: \expr handle -> case handle of
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
  , historyLength_max: 100
  }

