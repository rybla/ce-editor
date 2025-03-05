module Ui.Editor where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Editor (Editor)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Ui.Common (code, span, text)
import Ui.Console as Console

--------------------------------------------------------------------------------

data Query a = OtherQuery a

type Input = Editor

type M' = ExceptT PlainHTML M

type M = H.HalogenM State Action Slots Output Aff

type State =
  { editor :: Editor
  }

data Action
  = Initialize
  | OtherActions

type Slots :: Row Type
type Slots =
  ()

data Output = TellConsole (forall a. a -> Console.Query a)

--------------------------------------------------------------------------------

component = H.mkComponent { initialState, eval, render }
  where
  initialState editor = { editor }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = \action -> do
        state <- get
        handleAction action # runExceptT >>= flip either pure \err -> do
          put state
          trace "Editor Error" err
        pure unit
    }

  render _ =
    HH.div
      []
      [ text "{{Editor}}" ]

handleAction :: Action -> M' Unit
handleAction Initialize = do
  lift $ trace "Editor" $ text "initialized"
handleAction _ =
  throwError $ span [ code "handleAction", text ": unimplemented action" ]

--------------------------------------------------------------------------------

trace :: String -> PlainHTML -> M Unit
trace label content = H.raise $ TellConsole \a -> Console.AddMessage { label, content } a

