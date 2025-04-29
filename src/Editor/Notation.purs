module Editor.Notation where

import Prelude

import Control.Monad.Reader (ask, local)
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
  = All KidTokenOptions
  | Kid Int KidTokenOptions
  | Point Int
  | Punc (RenderM (Array (HTML w i)))

type KidTokenOptions =
  { indented :: Boolean
  }

defaultKidTokenOptions :: KidTokenOptions
defaultKidTokenOptions = bottom

parseString :: forall w i. String -> Array (Token w i)
parseString notation = notation
  # String.split (String.Pattern " ")
  # traverse parseWord
  # flip evalState { kid: 0, point: 0 }

parseWord :: forall w i. String -> State { kid :: Int, point :: Int } (Token w i)
parseWord "*" = pure $ All defaultKidTokenOptions
parseWord "\n*" = pure $ All defaultKidTokenOptions { indented = true }
parseWord "_" = do
  { kid: i } <- get
  prop (Proxy @"kid") %= (_ + 1)
  pure $ Kid i defaultKidTokenOptions
parseWord "\n_" = do
  { kid: i } <- get
  prop (Proxy @"kid") %= (_ + 1)
  pure $ Kid i defaultKidTokenOptions { indented = true }
parseWord "|" = do
  { point: i } <- get
  prop (Proxy @"point") %= (_ + 1)
  pure $ Point i
parseWord "\n" =
  pure $ Punc do
    ctx <- ask
    pure $ linebreak <> indentations ctx.indentLevel
parseWord str = pure $ Punc $ pure [ HH.div [ classes [ "Token punctuation" ] ] [ HH.text str ] ]

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
    All opt -> do
      ctx <- ask
      kids <- local (prop (Proxy @"indentLevel") (_ + 1)) do
        sequence args.kids
      pure $ fold
        [ if opt.indented then linebreak <> indentations ctx.indentLevel else []
        , Array.zipWith (\kid point -> [ point ] <> kid) kids args.points # fold
        , [ args.points # Array.last # fromMaybe do renderWarning $ "missing point #" <> show @Int (length args.points) ]
        ]
    Kid i opt -> do
      ctx <- ask
      kid <- local (prop (Proxy @"indentLevel") (_ + 1)) do
        args.kids Array.!! i # fromMaybe do pure [ renderWarning $ "missing kid #" <> show i ]
      pure $ fold
        [ if opt.indented then linebreak <> indentations ctx.indentLevel else []
        , kid
        ]
    Point i -> pure [ args.points Array.!! i # fromMaybe (renderWarning $ "missing point #" <> show i) ]
    Punc m_es -> m_es
  Right es -> es

linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "⇥" ] ]
indentations n = Array.replicate n indentation # fold

