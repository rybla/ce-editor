module Ui.App where

import Prelude

import Data.Array as Array
import Data.Expr (Expr(..), Index(..), Path, Point(..), Step(..), atSteps, getExtremeIndexes, getIndexesAroundStep, getStep)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Editor.Example.Editor1 (L(..), example_expr)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Ui.Common (doc)
import Ui.Component (Component)
import Ui.Component as Component
import Utility (todo)
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventListener, addEventListenerWithOptions, eventListener)
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

config =
  { initialExpr: example_expr {} 2 2
  }

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type PreLabel = L {}

type UiLabel = L
  { elem :: Element
  , eventListeners :: Array { eventType :: EventType, eventListener :: EventListener, capture :: Boolean }
  }

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

type State =
  { expr :: Expr UiLabel
  , handle :: Ref (Maybe Point)
  }

newState :: Effect State
newState = todo "newState"

main :: Effect Unit
main = do

  state <- newState

  pure unit

renderExpr :: Path -> Expr PreLabel -> Effect (Expr UiLabel)
renderExpr path (Expr expr) = do
  -- create element
  elem <- doc # Document.createElement "div"

  -- attributes
  elem # Element.setAttribute "class" "Expr"

  -- eventListeners
  eventListeners <- sequence
    [ do
        let eventType = EventType "click"
        let opts = { capture: true, once: false, passive: true }
        eventListener <- eventListener \_event -> do
          Console.log $ "clicked on Expr: " <> show path
        elem # Element.toEventTarget # addEventListenerWithOptions eventType eventListener opts
        pure { eventType, eventListener, capture: opts.capture }
    ]

  let meta = { elem, eventListeners }

  -- -- kids
  -- kids <- Expr expr # atSteps # traverse \{ outside: t, at: e } ->
  --   renderExpr (List.snoc path (t # getStep)) e

  -- -- append label
  -- do
  --   elem' <- createElement "div"
  --   elem' # Element.toNode # Node.setTextContent (show label)
  --   elem # appendChild elem'

  -- -- append points and kids
  -- kids # traverseWithIndex \(Expr { l: L _ meta' }) -> do
  --   elem_point <- renderPoint $ Point { path, j: ?a }
  --   elem # appendChild meta'.elem

  -- kids
  kids' <- Expr expr # atSteps # traverse \{ outside: t, at: kid } -> do
    let i = t # getStep
    let j = (i # getIndexesAroundStep)._L

    kid' <- renderExpr (List.snoc path i) kid
    elem # appendChild (((kid' # unwrap).l # unwrap).meta.elem)

    elem_point <- renderPoint $ Point { path, j }
    elem # appendChild elem_point

    pure kid'

  -- last point
  elem_point <- renderPoint $ Point { path, j: (Expr expr # getExtremeIndexes)._R }
  elem # appendChild elem_point

  pure $ Expr
    { l: expr.l # Newtype.over L _ { meta = meta }
    , kids: kids'
    }

renderPoint :: Point -> Effect Element
renderPoint = todo "renderPoint"

appendChild :: Element -> Element -> Effect Unit
appendChild child parent = parent # Element.toNode # Node.appendChild (child # Element.toNode)

createElement ∷ String → Effect Element
createElement tag = doc # Document.createElement tag