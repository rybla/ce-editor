module Ui.ViewExpr where

import Data.Expr
import Prelude
import Ui.Types

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Prelude (Proxy(..))
import Ui.Common (code, text)
import Ui.Common as Ui
import Ui.Console as Console
import Ui.ViewPoint (viewPoint_component)
import Utility (sortEquivalenceClasses)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

viewExpr_component :: H.Component ViewExprQuery ViewExprInput ViewExprOutput Aff
viewExpr_component = H.mkComponent { initialState: initialViewExprState, eval, render }
  where
  eval = H.mkEval H.defaultEval
    { initialize = pure Initialize_ViewExprAction
    , receive = pure <<< Receive_ViewExprAction
    , handleQuery = \query -> do
        state <- get
        handleViewExprQuery query # runExceptT >>= case _ of
          Left mb_err -> do
            put state
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewExprM [ "Error" ] err
            pure none
          Right a -> pure $ pure a
    , handleAction = \action -> do
        state <- get
        handleViewExprAction action # runExceptT >>= case _ of
          Left mb_err -> do
            case mb_err of
              Nothing -> pure unit
              Just err -> traceViewExprM [ "Error" ] err
            put state
          Right it -> pure it
    }

  render state =
    let
      Expr e0 = state.expr
    in
      HH.div
        [ Ui.classes [ "Expr" ]
        -- , HE.onMouseDown (StartDrag_ViewExprAction >>> ExprInteraction_ViewExprAction)
        -- , HE.onMouseMove (MidDrag_ViewExprAction >>> ExprInteraction_ViewExprAction)
        ]
        ( fold
            [ [ HH.div [ HP.classes [ HH.ClassName "ExprLabel" ] ]
                  [ case e0.l of
                      String s -> text s
                      Root -> text "root"
                  ]
              ]
            , let
                renderPoint i =
                  HH.slot (Proxy @"Point") i viewPoint_component
                    {}
                    (ViewPointOutput_ViewExprAction i)
                renderKid i e =
                  HH.slot (Proxy @"Expr") i viewExpr_component
                    { expr: e }
                    (ViewExprOutput_ViewExprAction i)
              in
                fold
                  [ e0.kids # foldMapWithIndex \i e ->
                      [ renderPoint (Index i)
                      , renderKid (Step i) e
                      ]
                  , [ renderPoint (Index (Array.length e0.kids)) ]
                  ]
            ]
        )

initialViewExprState :: ViewExprInput -> ViewExprState
initialViewExprState { expr } =
  { expr
  }

handleViewExprQuery :: forall a. ViewExprQuery a -> ViewExprM' a
handleViewExprQuery (ViewExprQuery qs_ a) = do
  let qss = qs_ # NEArray.toArray # sortEquivalenceClasses \(SingleViewExprQuery p1 _) (SingleViewExprQuery p2 _) -> p1 == p2
  qss # traverse_ \qs -> do
    let SingleViewExprQuery p _ = qs # NEArray.head
    case p of
      Nil -> qs # traverse_ handleSingleViewExprQuery
      i : is -> do
        let qs' = ViewExprQuery (qs <#> \(SingleViewExprQuery _ q') -> (SingleViewExprQuery is q')) unit
        H.query (Proxy @"Expr") i qs' # lift >>= case _ of
          Nothing -> throwError none
          Just it -> pure it
  pure a

handleSingleViewExprQuery :: SingleViewExprQuery -> ViewExprM' Unit
handleSingleViewExprQuery (SingleViewExprQuery _ (Modify_ViewExprQuery f)) = do
  modify_ f
handleSingleViewExprQuery (SingleViewExprQuery _ (ViewPointQuery_ViewExprQuery i pq)) = do
  H.query (Proxy @"Point") i pq # lift >>= case _ of
    Nothing -> throwError none
    Just it -> pure it

handleViewExprAction :: ViewExprAction -> ViewExprM' Unit
handleViewExprAction Initialize_ViewExprAction = pure unit
handleViewExprAction (Receive_ViewExprAction input) = do
  state <- get
  let state' = initialViewExprState input
  when (state /= state') do
    lift $ traceViewExprM [ "Receive" ] $ Ui.column
      [ Ui.text "receive"
      , Ui.list
          [ Ui.span [ text "expr' = ", code $ show state'.expr ]
          ]
      ]
    put state'
    pure unit
-- kid Expr stuff
handleViewExprAction (ViewExprOutput_ViewExprAction i (is /\ o)) = do
  H.raise ((i : is) /\ o) # lift
handleViewExprAction (ExprInteraction_ViewExprAction ei) = do
  case ei of
    Click_ViewExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    StartDrag_ViewExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    MidDrag_ViewExprAction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (Nil /\ ExprInteraction ei) # lift
  pure unit
-- Point stuff
handleViewExprAction (ViewPointOutput_ViewExprAction _i (Output_ViewPointOutput o)) =
  H.raise (Nil /\ Output_ViewExprOutput o) # lift
handleViewExprAction (ViewPointOutput_ViewExprAction i (ViewPointInteraction pi)) = do
  case pi of
    StartDrag_ViewPointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
    MidDrag_ViewPointInteraction event -> event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  H.raise (Nil /\ ViewPointInteraction_ViewExprOutput i pi) # lift

--------------------------------------------------------------------------------

traceViewExprM :: Array String -> PlainHTML -> ViewExprM Unit
traceViewExprM labels content = H.raise $ Tuple Nil $ Output_ViewExprOutput $ TellConsole \a -> Console.AddMessage { labels: [ "ViewExpr" ] <> labels, content } a

