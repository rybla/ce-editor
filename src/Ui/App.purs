module Ui.App where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Ui.Component (getKidsRef)
import Ui.Component as Component
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Aff Unit
main = do
  Console.log "main"
  liftEffect do
    win <- HTML.window
    doc <- Window.document win # map HTMLDocument.toDocument

    ----

    console <-
      Component.new doc
        { name: pure "console"
        , attributes: Map.fromFoldable
            [ "style" /\ "margin: 1em; border: 1px solid black; padding: 1em; display: flex; flex-direction: column;" ]
        } $
        Right []

    controls <-
      Component.new doc
        { name: pure "console"
        , attributes: Map.fromFoldable
            [ "style" /\ "margin: 1em; border: 1px solid black; padding: 1em; display: flex; flex-direction: column;" ]
        } $
        Right
          [ Component.new doc
              { name: pure "button_addLog"
              , tag: "button"
              , eventListeners:
                  [ { eventType: Event.EventType "click"
                    , eventListener: EventTarget.eventListener \_event -> do
                        n <- console # (getKidsRef >=> Ref.read >=> (Array.length >>> pure))
                        log <-
                          Component.new doc
                            { name: pure $ "log #" <> show n }
                            $ Left ("log #" <> show n)
                        console # Component.appendKid log
                    , once: false
                    , passive: false
                    , capture: false
                    }
                  ]
              } $
              Left "add a log to the console"
          , Component.new doc
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
              } $
              Left "remove a log from the console"
          ]

    ----

    Component.root
      [ controls
      , console
      ]
