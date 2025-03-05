module Ui.Widget where

import Prelude

import Control.Monad.State (get)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (liftEffect, modify_)
import Halogen as H
import Halogen.HTML (PlainHTML, fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ui.Common (scrollIntoView)
import Web.HTML.HTMLElement as HTMLElement

scrollToMe ∷ forall output m query input. MonadEffect m ⇒ H.Component query input output m
scrollToMe = H.mkComponent
  { initialState: const unit
  , eval: H.mkEval H.defaultEval
      { initialize = pure unit
      , handleAction = const do
          this # H.getHTMLElementRef >>= case _ of
            Nothing -> pure unit
            Just e -> e # HTMLElement.toElement # scrollIntoView # liftEffect
      }
  , render: const $ HH.div [ HP.ref this ] []
  }
  where
  this = H.RefLabel "this"

initializer
  ∷ forall query state output m
   . Monad m
  => H.Component query
       { initialState :: state
       , initialize :: m state
       , render :: state -> PlainHTML
       }
       output
       m
initializer = H.mkComponent
  { initialState: \{ initialState, initialize, render } ->
      { initialize
      , render
      , state: initialState
      }
  , eval: H.mkEval H.defaultEval
      { initialize = pure unit
      , handleAction = const do
          { initialize } <- get
          state <- initialize # lift
          modify_ _ { state = state }
      }
  , render: \state -> fromPlainHTML $ state.render state.state
  }

