module Ui.Console where

import Prelude

import Common (ConsoleMessage)
import Control.Monad.Writer (tell)
import Data.Array (filter, fold)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Halogen (modify_)
import Halogen as H
import Halogen.HTML (fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Type.Proxy (Proxy(..))
import Ui.Common (classes, style)
import Ui.Widget (scrollToMe)
import Ui.Widget as Widget

data Query a = AddMessage ConsoleMessage a

type State =
  { messages :: Array ConsoleMessage
  }

data Action

type Output = Void

disabledMessageLabels =
  [ -- "Drag"
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
    }

  handleQuery :: forall a. Query a -> _ (_ a)
  handleQuery (AddMessage m a) = do
    modify_ \state -> state { messages = state.messages `Array.snoc` m }
    pure (pure a)

  render state =
    HH.div
      [ classes [ "Console" ]
      , style do
          tell [ "flex-grow: 0", "flex-shrink: 0" ]
          tell [ "height: 40em" ]
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ classes [ "Header" ]
          , style do
              tell [ "padding: 0.5em" ]
              tell [ "background-color: black", "color: white" ]
          ]
          [ HH.text "Console" ]
      , HHK.div
          [ classes [ "Body" ]
          , style do
              tell [ "overflow-y: scroll" ]
              tell [ "padding: 0.5em" ]
              tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ]
          ] $ fold
          [ state.messages
              # filter (not <<< (_ `Array.elem` disabledMessageLabels) <<< _.label)
              # mapWithIndex \i m ->
                  Tuple (show i) $
                    HH.div
                      [ classes $ fold
                          [ [ "ConsoleMessage" ]
                          , if m.label # String.contains (String.Pattern "Error") then [ "ConsoleMessageError" ] else []
                          ]
                      ]
                      [ HH.div [ classes [ "ConsoleMessageLabel" ] ]
                          [ HH.text m.label ]
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
