module Ui.App1.Point where

import Prelude

import Control.Monad.State (get)
import Data.Array as Array
import Data.Lens ((%=))
import Data.Maybe (Maybe)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ui.App1.Common (PointAction(..), PointHTML, PointInput, PointM, PointOutput(..), PointQuery(..), PointSlots, PointState)
import Ui.Halogen (classes)
import Utility (prop)

component :: H.Component PointQuery PointInput PointOutput Aff
component = H.mkComponent { initialState, eval, render }

initialState :: PointInput -> PointState
initialState input =
  { point: input.point
  , mb_statuses: Set.empty
  }

eval :: forall a. H.HalogenQ PointQuery PointAction PointInput a -> H.HalogenM PointState PointAction PointSlots PointOutput Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_PointAction
  , handleQuery = handleQuery
  , handleAction = handleAction
  }

handleQuery :: forall a. PointQuery a -> PointM (Maybe a)
handleQuery (ModifyMaybeStatuses_PointQuery f a) = do
  -- get >>= \{ mb_statuses } -> Console.log $ "[Point] old mb_statuses: " <> show mb_statuses
  prop @"mb_statuses" %= f
  -- get >>= \{ mb_statuses } -> Console.log $ "[Point] new mb_statuses: " <> show mb_statuses
  pure (pure a)

handleAction :: PointAction -> PointM Unit
handleAction Initialize_PointAction = do
  Console.log "[Point] initialize"
handleAction (MouseDown_PointAction _event) = do
  state <- get
  H.raise $ MouseDown_PointOutput state.point
handleAction (MouseEnter_PointAction _event) = do
  state <- get
  H.raise $ MouseEnter_PointOutput state.point

render :: PointState -> PointHTML
render state =
  HH.div
    [ classes $ Array.fold
        [ [ "Point" ]
        , state.mb_statuses # Set.toUnfoldable # map show
        ]
    , HE.onMouseDown MouseDown_PointAction
    ]
    [ HH.text "" ]

