module Ui.App where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr (Expr(..), Path, Step(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Ui.Component (Component(..), getKidsRef)
import Ui.Component as Component
import Web.DOM (Document)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Aff Unit
main = do
  pure unit

editorComponent :: Effect Component
editorComponent = do
  let props = {}
  let kids = []
  Component.newTree props kids

exprComponent :: Path -> Expr -> Effect Component
exprComponent rev_path (Expr e) = do
  let
    props =
      { eventListeners:
          [ { eventType: EventType "click"
            , eventListener: EventTarget.eventListener \event -> do
                event # Event.stopPropagation
                Console.log $ "clicked on Expr at Path: " <> show (List.reverse rev_path)
            , passive: true
            , capture: false
            , once: false
            }
          ]
      }
  let
    cs_kids_exprs = e.kids # mapWithIndex \i -> exprComponent (Step i : rev_path)
    kids
      | Array.null e.kids =
          [ Component.newText
              {}
              $ show e.l
          ]
      | otherwise = Array.fold
          [ [ Component.newText {} $ show e.l
            , Component.newText {} $ "%"
            ]
          , cs_kids_exprs
          ]

  -- Array.fold
  Component.newTree props kids

