module Ui.Console where

import Prelude

import Common (ConsoleMessage, showConsoleMessageLabels)
import Control.Monad.Writer (tell)
import Data.Array (filter, fold)
import Data.Array as Array
import Data.Either (fromRight')
import Data.FunctorWithIndex (mapWithIndex)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Halogen (modify_)
import Halogen as H
import Halogen.HTML (fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Ui.Common (classes, style)
import Ui.Widget (scrollToMe)
import Ui.Widget as Widget
import Utility (impossible)

data Query a = AddMessage ConsoleMessage a

type State =
  { messages :: Array ConsoleMessage
  }

data Action = Clear

type Output = Void

disabledMessageLabelRegexes :: Array Regex
disabledMessageLabelRegexes = map (\s -> Regex.regex ("^" <> s <> "( / [^/]*)*$") mempty # fromRight' (impossible $ "invalid regex: " <> s))
  [ "Engine / Receive"
  , "Engine / Keyboard"
  , "Engine / Drag"
  , "Engine / Snapshot"
  , "Engine / Insert"
  , "ViewExpr"
  , "Engine / Move"
  ]

component ∷ forall input output. H.Component Query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State
  initialState _ =
    { messages: none
    }

  eval = H.mkEval H.defaultEval
    { handleQuery = handleQuery
    , handleAction = handleAction
    }

  handleQuery :: forall a. Query a -> _ (_ a)
  handleQuery (AddMessage m a) = do
    modify_ \st -> st { messages = st.messages `Array.snoc` m }
    pure (pure a)

  handleAction Clear = do
    modify_ \st -> st { messages = mempty }

  render state =
    HH.div
      [ classes [ "Console" ]
      , style do
          tell [ "flex-grow: 0", "flex-shrink: 0" ]
          tell [ "height: 30em" ]
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ classes [ "Header" ]
          , style do
              tell [ "padding: 0.5em" ]
              tell [ "background-color: black", "color: white" ]
              tell [ "display: flex", "flex-direction: row", "align-items: center", "justify-content: space-between" ]
          ]
          [ HH.div []
              [ HH.text "Console" ]
          , HH.div []
              [ HH.button [ HE.onClick $ const Clear ] [ HH.text "Clear" ] ]
          ]
      , HHK.div
          [ classes [ "Body" ]
          , style do
              tell [ "overflow-y: scroll" ]
              tell [ "padding: 0.5em" ]
              tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ]
          ] $ fold
          [ state.messages
              # filter (not <<< (\labels -> (_ `Regex.test` (labels # showConsoleMessageLabels)) `Array.any` disabledMessageLabelRegexes) <<< _.labels)
              # mapWithIndex \i m ->
                  Tuple (show i) $
                    HH.div
                      [ classes $ fold
                          [ [ "ConsoleMessage" ]
                          , if m.labels # Array.any (String.contains (String.Pattern "Error")) then [ "ConsoleMessageError" ] else []
                          ]
                      ]
                      [ HH.div [ classes [ "ConsoleMessageLabel" ] ]
                          [ HH.text $ m.labels # showConsoleMessageLabels ]
                      , let
                          content = m.content # fromPlainHTML
                        in
                          HH.slot_ (Proxy @"ConsoleMessageBody") i Widget.initializer
                            { initialState: true
                            , initialize: do
                                Aff.delay $ Milliseconds 100.0
                                pure false
                            , render: \new ->
                                HH.div
                                  [ classes $ fold
                                      [ [ "ConsoleMessageBody" ]
                                      , if new then [ "new" ] else []
                                      ]
                                  ]
                                  [ content ]
                            }
                      ]
          , [ let
                l = Array.length state.messages
              in
                Tuple (show l) $
                  HH.slot_ (Proxy @"scrollToMe") l scrollToMe unit
            ]
          ]
      ]
