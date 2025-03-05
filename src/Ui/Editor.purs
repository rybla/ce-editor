module Ui.Editor where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (fold, mapWithIndex)
import Data.Either (Either(..))
import Data.Expr (Expr(..))
import Data.Expr as Expr
import Data.List (List)
import Data.List as List
import Data.Unfoldable (none)
import Editor (Editor)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Prelude (Proxy(..))
import Ui.Common (style, text)
import Ui.Console as Console
import Utility (todo)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

--------------------------------------------------------------------------------

data Query a = OtherQuery a

type Input = Editor

type State =
  { editor :: Editor
  }

data Action
  = Initialize
  | ExprOutput_Action ExprOutput
  | EngineOutput_Action EngineOutput

type Slots =
  ( "Engine" :: H.Slot EngineQuery EngineOutput Unit
  , "Expr" :: H.Slot ExprQuery ExprOutput Unit
  )

data Output = TellConsole (forall a. a -> Console.Query a)

type M' = ExceptT PlainHTML M
type M = H.HalogenM State Action Slots Output Aff
type Html = H.ComponentHTML Action Slots M

component :: H.Component Query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState editor =
    { editor
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize
    , handleAction = \action -> do
        state <- get
        handleAction action # runExceptT >>= case _ of
          Left err -> do
            put state
            trace "Editor . Error" err
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
          , expr: state.editor.initial_expr
          }
          EngineOutput_Action
      , HH.slot (Proxy @"Expr") unit expr_component
          { expr: state.editor.initial_expr }
          ExprOutput_Action
      ]

handleAction :: Action -> M' Unit
handleAction Initialize = do
  lift $ trace "Editor" $ text "initialized"
handleAction (ExprOutput_Action (OutputExpr_Output _is o)) = do
  -- TODO: use `is` as index to expr that was source of output
  H.raise o # lift
handleAction (EngineOutput_Action (OutputEngine_Output o)) = do
  H.raise o # lift

--------------------------------------------------------------------------------

data EngineQuery a = OtherEngineQuery a

type EngineInput =
  { editor :: Editor
  , expr :: Expr
  }

type EngineState =
  { editor :: Editor
  , expr :: Expr
  }

data EngineAction
  = InitializeEngine
  | ReceiveEngine EngineInput

type EngineSlots = () :: Row Type

data EngineOutput = OutputEngine_Output Output

type EngineHTML = H.ComponentHTML EngineAction EngineSlots EngineM
type EngineM' = ExceptT PlainHTML EngineM
type EngineM = H.HalogenM EngineState EngineAction EngineSlots EngineOutput Aff

engine_component :: H.Component EngineQuery EngineInput EngineOutput Aff
engine_component = H.mkComponent { initialState: initialEngineState, eval, render }
  where
  eval = H.mkEval H.defaultEval
    { initialize = pure InitializeEngine
    , receive = pure <<< ReceiveEngine
    , handleQuery = \query -> do
        state <- get
        handleEngineQuery query # runExceptT >>= case _ of
          Left err -> do
            put state
            traceEngineM "Editor . Engine . Error" err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleEngineAction action # runExceptT >>= case _ of
          Left err -> do
            put state
            traceEngineM "Editor . Engine . Error" err
          Right it -> pure it
    }

  render = const $ HH.div [ style do tell [ "display: hidden" ] ] []

initialEngineState :: EngineInput -> EngineState
initialEngineState input =
  { editor: input.editor
  , expr: input.expr
  }

handleEngineQuery :: forall a. EngineQuery a -> EngineM' a
handleEngineQuery (OtherEngineQuery a) = pure a

handleEngineAction :: EngineAction -> EngineM' Unit
handleEngineAction InitializeEngine = do
  traceEngineM "Editor . Engine" (text "initialized") # lift
handleEngineAction (ReceiveEngine input) = do
  traceEngineM "Editor . Engine" (text "received") # lift
  put $ initialEngineState input

--------------------------------------------------------------------------------

data ExprQuery a = ModifyExprState (ExprState -> ExprState) a

type ExprInput =
  { expr :: Expr
  }

type ExprState =
  { expr :: Expr
  , ping :: Boolean
  }

data ExprAction
  = InitializeExpr
  | ReceiveExpr ExprInput
  | ExprOutput_ExprAction Int ExprOutput
  | ClickExpr MouseEvent

type ExprSlots =
  ( "Expr" :: H.Slot ExprQuery ExprOutput Int
  )

data ExprOutput = OutputExpr_Output (List Int) Output

type ExprHTML = H.ComponentHTML ExprAction ExprSlots ExprM
type ExprM' = ExceptT PlainHTML ExprM
type ExprM = H.HalogenM ExprState ExprAction ExprSlots ExprOutput Aff

expr_component :: H.Component ExprQuery ExprInput ExprOutput Aff
expr_component = H.mkComponent { initialState: initialExprState, eval, render }
  where
  eval = H.mkEval H.defaultEval
    { initialize = pure InitializeExpr
    , receive = pure <<< ReceiveExpr
    , handleQuery = \query -> do
        state <- get
        handleExprQuery query # runExceptT >>= case _ of
          Left err -> do
            put state
            traceExprM "Editor . Expr . Error" err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleExprAction action # runExceptT >>= case _ of
          Left err -> do
            put state
            traceExprM "Editor . Expr . Error" err
          Right it -> pure it
    }

  render
    { expr: Expr l es
    , ping
    } =
    HH.div
      [ HP.classes $ [ [ HH.ClassName "Expr" ], if ping then [ H.ClassName "ping" ] else [] ] # fold
      , HE.onClick ClickExpr
      ]
      ( fold
          [ [ HH.div [ HP.classes [ HH.ClassName "ExprLabel" ] ]
                [ case l of
                    Expr.String s -> text s
                ]
            ]
          , es
              # mapWithIndex \i e ->
                  HH.slot (Proxy @"Expr") i expr_component
                    { expr: e }
                    (ExprOutput_ExprAction i)
          ]
      )

initialExprState :: ExprInput -> ExprState
initialExprState { expr } =
  { expr
  , ping: false
  }

handleExprQuery :: forall a. ExprQuery a -> ExprM' a
handleExprQuery (ModifyExprState f a) = do
  modify_ f
  pure a

handleExprAction :: ExprAction -> ExprM' Unit
handleExprAction InitializeExpr = pure unit
handleExprAction (ReceiveExpr input) = do
  state <- get
  let state' = initialExprState input
  when (state /= state') do
    put state
    pingExpr
handleExprAction (ClickExpr event) = do
  event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  pingExpr
handleExprAction (ExprOutput_ExprAction i (OutputExpr_Output is o)) = do
  H.raise (OutputExpr_Output (List.Cons i is) o) # lift

pingExpr :: ExprM' Unit
pingExpr = do
  modify_ _ { ping = true }
  Aff.delay (Milliseconds 500.0) # liftAff
  modify_ _ { ping = false }

--------------------------------------------------------------------------------

trace :: String -> PlainHTML -> M Unit
trace label content = H.raise $ TellConsole \a -> Console.AddMessage { label, content } a

traceExprM :: String -> PlainHTML -> ExprM Unit
traceExprM label content = H.raise $ OutputExpr_Output none $ TellConsole \a -> Console.AddMessage { label, content } a

traceEngineM :: String -> PlainHTML -> EngineM Unit
traceEngineM label content = H.raise $ OutputEngine_Output $ TellConsole \a -> Console.AddMessage { label, content } a

