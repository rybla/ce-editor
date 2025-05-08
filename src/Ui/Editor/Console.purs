module Ui.Editor.Console where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, modify_)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Ui.Editor.Common (ConsoleAction(..), ConsoleHTML, ConsoleInput, ConsoleM, ConsoleOutput, ConsoleQuery, ConsoleSlots, ConsoleState)
import Ui.Editor.Console.ConsoleItem as ConsoleItem
import Ui.Editor.Console.Messages as Console.Messages
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

delay_update = Milliseconds 200.0

--------------------------------------------------------------------------------

component :: H.Component ConsoleQuery ConsoleInput ConsoleOutput Aff
component = H.mkComponent { initialState, eval, render }

initialState :: ConsoleInput -> ConsoleState
initialState _input =
  { messages: none
  , timestamp: 0.0
  }

eval :: forall a. H.HalogenQ ConsoleQuery ConsoleAction ConsoleInput a -> H.HalogenM ConsoleState ConsoleAction ConsoleSlots ConsoleOutput Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_ConsoleAction
  , handleAction = handleAction
  }

handleAction :: ConsoleAction -> ConsoleM Unit
handleAction Initialize_ConsoleAction = do
  void $ H.subscribe =<< timer delay_update Tick_ConsoleAction
  pure unit
handleAction Tick_ConsoleAction = do
  state <- get
  timestamp <- Console.Messages.get_timestamp # liftEffect
  messages <- Console.Messages.get_messages # liftEffect
  when (state.timestamp /= timestamp) do
    Console.log "[Console] rerender"
    modify_ _
      { messages = messages
      , timestamp = timestamp
      }

timer :: forall m a. MonadAff m => Milliseconds -> a -> m (HS.Emitter a)
timer delay val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay delay
    H.liftEffect $ HS.notify listener val
  pure emitter

render :: ConsoleState -> ConsoleHTML
render state =
  HHK.div [ classes [ "Console" ] ] $ state.messages # map \item ->
    show item.timestamp /\ HH.slot_ (Proxy @"ConsoleItem") item.timestamp ConsoleItem.component { item }

