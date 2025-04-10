module Ui.Example.List where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Ui.Component (getKidsRef)
import Ui.Component as Component
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget

main :: Effect Unit
main = do
  Console.log "main"

  console <-
    Component.newTree
      { name: pure "console"
      , attributes: Map.fromFoldable
          [ "style" /\ "margin: 1em; border: 1px solid black; padding: 1em; display: flex; flex-direction: column;" ]
      }
      []

  controls <-
    Component.newTree
      { name: pure "console"
      , attributes: Map.fromFoldable
          [ "style" /\ "margin: 1em; border: 1px solid black; padding: 1em; display: flex; flex-direction: column;" ]
      }
      [ Component.newText
          { name: pure "button_addLog"
          , tag: "button"
          , eventListeners:
              [ { eventType: Event.EventType "click"
                , eventListener: EventTarget.eventListener \_event -> do
                    n <- console # (getKidsRef >=> Ref.read >=> (Array.length >>> pure))
                    log <-
                      Component.newText
                        { name: pure $ "log #" <> show n }
                        $ "log #" <> show n
                    console # Component.appendKid log
                , once: false
                , passive: false
                , capture: false
                }
              ]
          }
          "add a log to the console"
      , Component.newText
          { name: pure "button_addLog"
          , tag: "button"
          , eventListeners:
              [ { eventType: Event.EventType "click"
                , eventListener: EventTarget.eventListener \_event -> do
                    n <- console # (getKidsRef >=> Ref.read >=> (Array.length >>> pure))
                    let i = n / 2
                    unless (n == 0) do
                      console # Component.removeKid i
                , once: false
                , passive: false
                , capture: false
                }
              ]
          }
          "remove a log from the console"
      ]

  ----

  Component.root
    [ pure controls
    , pure console
    ]

