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
import Data.Unfoldable (none)
import Editor (Editor)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Type.Prelude (Proxy(..))
import Ui.Common (code, span, style, text)
import Ui.Console as Console

--------------------------------------------------------------------------------

data Query a = OtherQuery a

type Input = Editor

type State =
  { editor :: Editor
  , expr :: Expr
  }

data Action
  = Initialize
  | ExprOutputAction ExprOutput
  | OtherActions

type Slots =
  ( "Expr" :: H.Slot ExprQuery ExprOutput Unit
  )

data Output = TellConsole (forall a. a -> Console.Query a)

type M' = ExceptT PlainHTML M
type M = H.HalogenM State Action Slots Output Aff
type Html = H.ComponentHTML Action Slots M

component = H.mkComponent { initialState, eval, render }
  where
  initialState editor =
    { editor
    , expr: editor.initial_expr
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
      [ HH.slot (Proxy @"Expr") unit expr_component
          { expr: state.expr }
          ExprOutputAction
      ]

handleAction :: Action -> M' Unit

handleAction Initialize = do
  lift $ trace "Editor" $ text "initialized"

handleAction (ExprOutputAction (OutputExprOutput o)) = do
  H.raise o # lift

handleAction _ =
  throwError $ span [ code "handleAction", text ": unimplemented action" ]

--------------------------------------------------------------------------------

data ExprQuery a = ModifyExprState (ExprState -> ExprState) a

type ExprInput =
  { expr :: Expr
  }

type ExprState =
  { expr :: Expr
  }

data ExprAction
  = InitializeExpr
  | ReceiveExpr ExprState
  | ExprOutputExprAction ExprOutput

type ExprSlots =
  ( "Expr" :: H.Slot ExprQuery ExprOutput Int
  )

data ExprOutput = OutputExprOutput Output

type ExprHTML = H.ComponentHTML ExprAction ExprSlots ExprM
type ExprM' = ExceptT PlainHTML ExprM
type ExprM = H.HalogenM ExprState ExprAction ExprSlots ExprOutput Aff

expr_component :: H.Component ExprQuery ExprInput ExprOutput Aff
expr_component = H.mkComponent { initialState, eval, render }
  where
  initialState = identity

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
    } =
    HH.div
      [ style do
          tell [ "box-shadow: 0 0 0 1px black" ]
          tell [ "padding: 0.5em" ]
          tell [ "display: inline-flex", "flex-direction: row", "gap: 0.5em" ]
      ] $ fold
      [ [ HH.div
            [ style do
                tell [ "font-weight: bold" ]
            ]
            [ case l of
                Expr.String s -> text s
            ]
        ]
      , es
          # mapWithIndex \i e -> HH.slot (Proxy @"Expr") i expr_component
              { expr: e }
              ExprOutputExprAction
      ]

handleExprQuery :: forall a. ExprQuery a -> ExprM' a

handleExprQuery (ModifyExprState f a) = do
  modify_ f
  pure a

handleExprAction :: ExprAction -> ExprM' Unit

handleExprAction InitializeExpr = do
  lift $ traceExprM "Editor . Expr" $ text "initialized"

handleExprAction (ReceiveExpr state) = put state

-- TODO: include index that gradually gets built up
handleExprAction (ExprOutputExprAction o) = H.raise o # lift

--------------------------------------------------------------------------------

data ExprStyle

--------------------------------------------------------------------------------

-- type RenderCtx = {}

-- initialRenderCtx :: State -> RenderCtx
-- initialRenderCtx _ = {}

-- type RenderM = Reader RenderCtx

-- renderExpr :: Expr -> RenderM Html
-- renderExpr (Expr l es) = do
--   htmls_es <- es # traverse renderExpr
--   pure $
--     HH.div
--       []
--       []

--------------------------------------------------------------------------------

trace :: String -> PlainHTML -> M Unit
trace label content = H.raise $ TellConsole \a -> Console.AddMessage { label, content } a

traceExprM :: String -> PlainHTML -> ExprM Unit
traceExprM label content = H.raise $ OutputExprOutput $ TellConsole \a -> Console.AddMessage { label, content } a

