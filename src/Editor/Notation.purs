module Editor.Notation where

import Prelude

import Control.Monad.State (State, evalState, get)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (fold, foldMap, length)
import Data.Lens ((%=))
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.Traversable (traverse)
import Editor.Common (AssembleExpr, renderWarning)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Ui.Halogen (classes)

data Token
  = All
  | Kid Int
  | Point Int
  | Punc (Array PlainHTML)

parseString :: String -> Array Token
parseString notation = notation
  # String.split (String.Pattern " ")
  # traverse parseWord
  # flip evalState { kid: 0, point: 0 }

parseWord :: String -> State { kid :: Int, point :: Int } Token
parseWord "*" = pure All
parseWord "_" = do
  { kid: i } <- get
  prop (Proxy @"kid") %= (_ + 1)
  pure $ Kid i
parseWord "|" = do
  { point: i } <- get
  prop (Proxy @"point") %= (_ + 1)
  pure $ Point i
parseWord "\n" = pure $ Punc [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
parseWord "\t" = pure $ Punc [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⇥" ] ]
parseWord str = pure $ Punc [ HH.div [ classes [ "Token punctuation" ] ] [ HH.text str ] ]

mkAssembleExpr :: forall l. (l -> Array Token \/ Array PlainHTML) -> AssembleExpr l
mkAssembleExpr getTokens { label, kids, points } = case label # getTokens of
  Left tokens -> tokens # foldMap case _ of
    All -> fold
      [ Array.zipWith (\kid point -> [ point ] <> kid) kids points # fold
      , [ points # Array.last # fromMaybe (renderWarning $ "missing point #" <> show @Int (length points)) ]
      ]
    Kid i -> kids Array.!! i # fromMaybe [ renderWarning $ "missing kid #" <> show i ]
    Point i -> [ points Array.!! i # fromMaybe (renderWarning $ "missing point #" <> show i) ]
    Punc es -> es <#> HH.fromPlainHTML
  Right es -> es <#> HH.fromPlainHTML

