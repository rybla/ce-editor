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
import Data.Traversable (sequence, traverse)
import Editor.Common (RenderM, renderWarning)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Ui.Halogen (classes)

data Token w i
  = All
  | Kid Int
  | Point Int
  | Punc (Array (HTML w i))

parseString :: forall w i. String -> Array (Token w i)
parseString notation = notation
  # String.split (String.Pattern " ")
  # traverse parseWord
  # flip evalState { kid: 0, point: 0 }

parseWord :: forall w i. String -> State { kid :: Int, point :: Int } (Token w i)
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

mkAssembleExpr
  :: forall l w i
   . ( { label :: l
       , kids :: Array (RenderM (Array (HTML w i)))
       , points :: Array (HTML w i)
       }
       -> (Array (Token w i) \/ RenderM (Array (HTML w i)))
     )
  -> { label :: l
     , kids :: Array (RenderM (Array (HTML w i)))
     , points :: Array (HTML w i)
     }
  -> RenderM (Array (HTML w i))
mkAssembleExpr getTokens args = case args # getTokens of
  Left tokens -> tokens # foldMap case _ of
    All -> do
      kids <- sequence args.kids
      pure $ fold
        [ Array.zipWith (\kid point -> [ point ] <> kid) kids args.points # fold
        , [ args.points # Array.last # fromMaybe do renderWarning $ "missing point #" <> show @Int (length args.points) ]
        ]
    Kid i -> args.kids Array.!! i # fromMaybe do pure [ renderWarning $ "missing kid #" <> show i ]
    Point i -> pure [ args.points Array.!! i # fromMaybe (renderWarning $ "missing point #" <> show i) ]
    Punc es -> pure es
  Right es -> es

