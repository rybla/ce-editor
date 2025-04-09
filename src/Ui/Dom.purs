module Ui.Dom where

import Prelude
import Web.DOM

import Data.Foldable (traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Effect (Effect)
import Prim.Row (class Nub, class Union)
import Record as Record
import Web.DOM.Document as DOM.Document
import Web.DOM.Element as DOM.Element
import Web.DOM.Node as DOM.Node
import Web.Event.Event (EventType)
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget

type ElementOpts =
  ( tag :: String
  , classes :: Array String
  , attributes :: ElementAttributes
  , eventListeners :: Array { capture :: Boolean, eventListener :: Effect EventTarget.EventListener, eventType :: EventType, once :: Boolean, passive :: Boolean }
  )

type ElementAttributes = Map String String

type ElementEventListener =
  { eventType :: EventType
  , eventListener :: Effect EventTarget.EventListener
  -- Whether or not to dispatch event to this listener before dispatching to listeners below this node in the DOM tree.
  -- Default is `false`.
  , capture :: Boolean
  -- Whether or not this listener can be invoked at most once.
  -- If true, then is automatically removed after first invokation).
  -- Default is `false`.
  , once :: Boolean
  -- Whether or not the listener cannot call `preventDefault`.
  -- Default is `false`.
  , passive :: Boolean
  }

element
  ∷ ∀ (opts ∷ Row Type) (opts' ∷ Row Type)
  . Union opts ElementOpts opts'
  ⇒ Nub opts' ElementOpts
  ⇒ Document
  → Record opts
  → Array (Effect Element)
  → Effect Element
element doc opts_ kids = do
  let
    opts = Record.merge opts_
      { tag: "div"
      , classes: [] :: Array String
      , attributes: Map.empty :: ElementAttributes
      , eventListeners: [] :: Array ElementEventListener
      }
  -- create
  e <- doc # DOM.Document.createElement opts.tag
  -- classes
  e # DOM.Element.setClassName (opts.classes # String.joinWith " ")
  -- attributes
  opts.attributes # traverseWithIndex_ \k v -> do
    e # DOM.Element.setAttribute k v
  -- listeners
  opts.eventListeners # traverse_ \l -> do
    eventListener <- l.eventListener
    e # DOM.Element.toEventTarget
      # EventTarget.addEventListenerWithOptions l.eventType eventListener
          { capture: l.capture, once: l.once, passive: l.passive }
  -- kids
  kids # traverse_ \m_kid -> do
    e_kid <- m_kid
    e # DOM.Element.toNode # DOM.Node.appendChild (DOM.Element.toNode e_kid)
  pure e

element_ex1 doc = element doc
  { eventListeners:
      [ { eventType: Event.EventType "click"
        , eventListener: EventTarget.eventListener \_event -> do
            pure unit
        , capture: false
        , once: false
        , passive: false
        }
      ]
  }
  [ element doc {} []
  , element doc {} []
  , element doc {} []
  ]

