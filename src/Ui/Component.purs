module Ui.Component where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prim.Row (class Nub, class Union)
import Record as Record
import Ui.Common (getText_Element, setText_Element)
import Utility (fromMaybeM)
import Web.DOM as DOM
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as HTMLWindow

--------------------------------------------------------------------------------

newtype Component = Component
  { name :: Maybe String
  , element :: DOM.Element
  , kids :: Maybe (Ref (Array Component))
  , eventListeners :: Array { eventType :: Event.EventType, eventListener :: EventTarget.EventListener, capture :: Boolean }
  }

derive instance Newtype Component _

instance Show Component where
  show (Component c) = "<" <> label <> " / " <> ty <> ">"
    where
    label = case c.name of
      Just name -> name
      Nothing -> "Component"
    ty = case c.kids of
      Nothing -> "text"
      Just _ -> "tree"

type ComponentOpts =
  ( name :: Maybe String
  , tag :: String
  , classes :: Array String
  , attributes :: ComponentAttributes
  , eventListeners :: Array { capture :: Boolean, eventListener :: Effect EventTarget.EventListener, eventType :: Event.EventType, once :: Boolean, passive :: Boolean }
  )

type ComponentAttributes = Map String String

type ComponentEventListener =
  { eventType :: Event.EventType
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

root :: Array Component -> Effect Unit
root kids = do
  win <- HTML.window
  doc <- win # HTMLWindow.document
  Component c <-
    new (doc # HTMLDocument.toDocument)
      { name: pure "root", classes: [ "root" ] } $
      pure (kids # map pure)
  body <- doc # HTMLDocument.body >>= fromMaybeM do throwError $ Exception.error $ "no body"
  body # HTMLElement.toNode # Node.appendChild (c.element # Element.toNode)

new
  ∷ ∀ (opts ∷ Row Type) (opts' ∷ Row Type)
  . Union opts ComponentOpts opts'
  ⇒ Nub opts' ComponentOpts
  ⇒ DOM.Document
  → Record opts
  → String \/ Array (Effect Component)
  → Effect Component
new doc opts_ content = do
  let
    opts = Record.merge opts_
      { name: Nothing @String
      , tag: "div"
      , classes: [] :: Array String
      , attributes: Map.empty :: ComponentAttributes
      , eventListeners: [] :: Array ComponentEventListener
      }
  -- create
  e <- doc # Document.createElement opts.tag
  -- classes
  e # Element.setClassName (opts.classes # String.joinWith " ")
  -- attributes
  opts.attributes # traverseWithIndex_ \k v -> do
    e # Element.setAttribute k v
  -- listeners
  eventListeners <- opts.eventListeners # traverse \l -> do
    eventListener <- l.eventListener
    e # Element.toEventTarget
      # EventTarget.addEventListenerWithOptions l.eventType eventListener
          { capture: l.capture, once: l.once, passive: l.passive }
    pure { eventType: l.eventType, eventListener, capture: l.capture }
  -- kids
  kids' <- case content of
    Left str -> do
      Node.setTextContent str (e # Element.toNode)
      pure Nothing
    Right es_kids -> do
      cs_kids <- es_kids # traverse \m_kid -> do
        Component c_kid <- m_kid
        e # Element.toNode # Node.appendChild (c_kid.element # Element.toNode)
        pure (Component c_kid)
      Just <$> Ref.new cs_kids
  -- kids_ref <- Ref.new kids
  pure $ Component
    { name: opts.name
    , element: e
    , kids: kids'
    , eventListeners
    }

getKidsRef :: Component -> Effect (Ref (Array Component))
getKidsRef (Component c) = case c.kids of
  Nothing -> throwError $ Exception.error $ "getKidsRef " <> show (Component c)
  Just kids -> pure kids

getText :: Component -> Effect String
getText (Component c) = case c.kids of
  Nothing -> c.element # getText_Element
  Just _ -> throwError $ Exception.error $ "getText " <> show (Component c)

setText :: String -> Component -> Effect Unit
setText str (Component c) = case c.kids of
  Nothing -> c.element # setText_Element str
  Just _ -> throwError $ Exception.error $ "setText " <> show (Component c)

addKid :: Int -> Component -> Component -> Effect Unit
addKid i (Component c_kid) (Component c) = do
  kidsRef <- Component c # getKidsRef
  kids <- kidsRef # Ref.read
  if i < Array.length kids - 1 then do
    Component c_kid_after <- kids Array.!! (i + 1) # fromMaybeM do throwError $ Exception.error $ "addKid " <> show i <> " " <> show (Component c)
    -- add kid as child of this component's element
    c.element
      # Element.toNode
      # Node.insertBefore (c_kid.element # Element.toNode) (c_kid_after.element # Element.toNode)
    -- add kid to this component's kids
    kids' <- kids # Array.insertAt i (Component c_kid) # fromMaybeM do
      throwError $ Exception.error $ "addKid " <> show i <> " " <> show (Component c)
    kidsRef # Ref.write kids'
  else {- i == Array.length kids -}  do
    -- add kid as child of this component's element
    c.element # Element.toNode # Node.appendChild (c_kid.element # Element.toNode)
    -- add kid to this component's kids
    let kids' = kids `Array.snoc` Component c_kid
    kidsRef # Ref.write kids'

appendKid :: Component -> Component -> Effect Unit
appendKid kid c = do
  kids <- c # (getKidsRef >=> Ref.read)
  c # addKid (Array.length kids) kid

replaceKid :: Int -> (Array Component -> Effect Component) -> Component -> Effect Unit
replaceKid i adopt (Component c) = do
  kidsRef <- Component c # getKidsRef
  kids <- kidsRef # Ref.read
  -- get kid
  Component c_kid <- kids Array.!! i # fromMaybeM do
    throwError $ Exception.error $ "removeKid " <> show i <> " " <> show (Component c)
  -- kid' adopts kid's kids
  kid' <- adopt =<< (Component c_kid # (getKidsRef >=> Ref.read))
  -- remove kid's eventListeners
  c_kid.eventListeners # traverse_ \el ->
    c_kid.element # Element.toEventTarget # EventTarget.removeEventListener el.eventType el.eventListener el.capture
  -- remove kid as child of this component's element
  c.element # Element.toNode # Node.removeChild (c_kid.element # Element.toNode)
  -- replace kid with kid' in this component's kids
  kids' <- kids # Array.modifyAt i (const kid') # fromMaybeM do
    throwError $ Exception.error $ "replaceKid " <> show i <> " " <> show (Component c)
  kidsRef # Ref.write kids'

removeKid :: Int -> Component -> Effect Unit
removeKid i (Component c) = do
  Console.log $ "removeKid " <> show i <> " " <> show (Component c)
  kidsRef <- Component c # getKidsRef
  kids <- kidsRef # Ref.read
  -- get kid
  Component c_kid <- kids Array.!! i # fromMaybeM do
    throwError $ Exception.error $ "removeKid " <> show i <> " " <> show (Component c)
  -- remove kid's descendants
  Component c_kid # removeDescendants
  -- remove kid's eventListeners
  c_kid.eventListeners # traverse_ \el ->
    c_kid.element # Element.toEventTarget # EventTarget.removeEventListener el.eventType el.eventListener el.capture
  -- remove kid as child of this component's element
  Console.log $ "removeKid " <> show (Component c_kid) <> " " <> show (Component c)
  c.element # Element.toNode # Node.removeChild (c_kid.element # Element.toNode)
  -- remove kid from this component's kids
  kids' <- kids # Array.deleteAt i # fromMaybeM do
    throwError $ Exception.error $ "replaceKid " <> show i <> " " <> show (Component c)
  kidsRef # Ref.write kids'

removeDescendants :: Component -> Effect Unit
removeDescendants (Component c) = case c.kids of
  Nothing -> pure unit
  Just kidsRef ->
    kidsRef # Ref.read >>= traverseWithIndex_ \i _ ->
      Component c # removeKid i

--------------------------------------------------------------------------------

component_ex1 doc = new doc
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
  ( pure
      [ new doc {} (pure [])
      , new doc {} (pure [])
      , new doc {} (pure [])
      ]
  )

