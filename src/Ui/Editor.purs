module Ui.Editor where

import Data.Expr
import Prelude
import Ui.Types

import Control.Monad.Except (runExceptT)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.Query.Event as HQE
import Type.Prelude (Proxy(..))
import Ui.Common (style, text)
import Ui.Console as Console
import Ui.Engine (engine_component)
import Ui.ViewExpr (viewExpr_component)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as HTML.Window
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEventType

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

editor_component :: H.Component EditorQuery EditorInput EditorOutput Aff
editor_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: EditorInput -> EditorState
  initialState editor =
    { editor
    , expr: Root % editor.initial_exprs
    , max_history_length: 100
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = \action -> do
        state <- get
        handleAction action # runExceptT >>= case _ of
          Left err -> do
            put state
            traceEditorM [ "Error" ] err
          Right it -> pure it
    }

  render state =
    HH.div
      [ style do
          tell [ "flex-grow: 1", "flex-shrink: 1" ]
          tell [ "overflow: scroll" ]
          tell [ "padding: 0.5em" ]
      ]
      [ HH.slot (Proxy @"Engine") unit engine_component
          { editor: state.editor
          , expr: state.expr
          , handle: state.editor.initial_handle
          }
          EngineOutput_Action
      , HH.slot (Proxy @"Expr") unit viewExpr_component
          { expr: state.expr
          }
          ViewExprOutput_Action
      ]

handleAction :: EditorAction -> EditorM' Unit
handleAction Initialize = do
  lift $ traceEditorM [ "Initialize" ] $ text "initialize"
  doc <- liftEffect $ HTML.Window.document =<< HTML.window
  lift $ H.subscribe' \_sub_id -> HQE.eventListener MouseEventType.mouseup (HTMLDocument.toEventTarget doc) $ MouseEvent.fromEvent >=> \_e -> do
    pure $ EngineQuery_Action EndDrag_EngineQuery
handleAction (EngineQuery_Action q) = do
  lift $ H.tell (Proxy @"Engine") unit q
handleAction (EngineOutput_Action eo) = case eo of
  Output_EngineOutput output -> H.raise output # lift
  ViewExprQuery_EngineOutput query -> H.tell (Proxy @"Expr") unit query # lift
  SetExpr_EngineOutput expr' -> modify_ _ { expr = expr' }
-- SetBufferEnabled_ViewPointQuery_EngineOutput bufferEnabled' -> do
--   todo "tell the appropriate ViewPoint to modify its bufferEnabled"
handleAction (ViewExprOutput_Action (path /\ eo)) = case eo of
  Output_ViewExprOutput o ->
    H.raise o # lift
  ExprInteraction pi -> do
    lift $ H.tell (Proxy @"Engine") unit (ExprInteraction_EngineQuery path pi)
  ViewPointInteraction_ViewExprOutput j pi ->
    lift $ H.tell (Proxy @"Engine") unit (ViewPointInteraction_EngineQuery (Point { path, j }) pi)

--------------------------------------------------------------------------------

traceEditorM :: Array String -> PlainHTML -> EditorM Unit
traceEditorM labels content = H.raise $ TellConsole \a -> Console.AddMessage { labels: [ "Editor" ] <> labels, content } a

