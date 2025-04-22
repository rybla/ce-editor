module Ui.App1.Point where

import Prelude

import Control.Monad.State (get, gets, modify_, put)
import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Lens ((%=))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ui.App1.Common (PointAction(..), PointHTML, PointInput, PointM, PointOutput(..), PointQuery(..), PointSlots, PointState, PointStatus(..), Buffer)
import Ui.Element as Element
import Ui.Halogen (classes)
import Utility (prop)
import Web.HTML.HTMLElement as HTMLElement

component :: H.Component PointQuery PointInput PointOutput Aff
component = H.mkComponent { initialState, eval, render }

initialState :: PointInput -> PointState
initialState input =
  { point: input.point
  , statuses: Set.empty
  , mb_buffer: none
  }

eval :: forall a. H.HalogenQ PointQuery PointAction PointInput a -> H.HalogenM PointState PointAction PointSlots PointOutput Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_PointAction
  , handleQuery = handleQuery
  , handleAction = handleAction
  , receive = pure <<< Receive_PointAction
  }

handleQuery :: forall a. PointQuery a -> PointM (Maybe a)
handleQuery (ModifyStatuses_PointQuery f a) = do
  -- get >>= \{ statuses } -> Console.log $ "[Point] old statuses: " <> show statuses
  prop @"statuses" %= f
  -- get >>= \{ statuses } -> Console.log $ "[Point] new statuses: " <> show statuses
  gets _.statuses >>= \statuses -> do
    when (not $ statuses # Set.intersection ss_Focus # Set.isEmpty) do
      mb_elem_this <- H.getHTMLElementRef this
      case mb_elem_this of
        Nothing -> pure unit
        Just elem_this -> liftEffect $ elem_this # HTMLElement.toElement # Element.scrollIntoView
  pure $ pure a
handleQuery (SetBufferIsOpen b a) = do
  if b then
    modify_ \state -> state { mb_buffer = pure $ state.mb_buffer # fromMaybe initialBuffer }
  else
    modify_ _ { mb_buffer = none }
  pure $ pure a
handleQuery (GetBufferIsOpen k) = do
  state <- get
  pure $ pure $ k $ state.mb_buffer # isJust

initialBuffer :: Buffer
initialBuffer =
  { query: ""
  , results: []
  }

ss_Focus = Set.fromFoldable [ Point_Handle_PointStatus, LeftFocus_PointStatus, RightFocus_PointStatus ]

handleAction :: PointAction -> PointM Unit
handleAction Initialize_PointAction = do
  -- Console.log "[Point] initialize"
  pure unit
handleAction (Receive_PointAction input) = do
  put $ initialState input
handleAction (MouseDown_PointAction event) = do
  state <- get
  H.raise $ MouseDown_PointOutput event state.point
handleAction (MouseEnter_PointAction event) = do
  state <- get
  H.raise $ MouseEnter_PointOutput event state.point

render :: PointState -> PointHTML
render state =
  HH.div
    [ HP.ref this
    , classes $ Array.fold
        [ [ "Point" ]
        , state.statuses # Set.toUnfoldable # map show
        ]
    , HE.onMouseDown MouseDown_PointAction
    , HE.onMouseEnter MouseEnter_PointAction
    ]
    [ HH.div [ classes $ fold [ [ "Buffer" ], state.mb_buffer # foldMap (const [ "open" ]) ] ]
        [ HH.text "{{Buffer}}" ]
    , HH.div [ classes [ "Left" ] ] []
    , HH.div [ classes [ "Middle" ] ] []
    , HH.div [ classes [ "Right" ] ] []
    ]

this = H.RefLabel "this"

