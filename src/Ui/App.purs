module Ui.App where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr (Expr(..))
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Ui.Component (Component(..), getKidsRef)
import Ui.Component as Component
import Web.DOM (Document)
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Aff Unit
main = do
  pure unit

-- editorComponent :: Effect Component
-- editorComponent = do
--   let props = {}
--   let kids = []
--   Component.new props $ Right kids

-- exprComponent ::  Expr -> Effect Component
-- exprComponent doc (Expr e) = do
--   let props = {}
--   let kids = []
--   Component.new doc props $ Right kids

