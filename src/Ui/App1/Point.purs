module Ui.App1.Point where

import Prelude

import Control.Monad.State (get, gets, modify_, put)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Lens ((%=))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Ui.App1.Buffer as Buffer
import Ui.App1.Common (PointAction(..), PointHTML, PointInput, PointM, PointOutput(..), PointQuery(..), PointSlots, PointState, PointStatus(..))
import Ui.Element as Element
import Ui.Halogen (classes)
import Utility (prop)
import Web.HTML.HTMLElement as HTMLElement

component :: forall l. Show l => H.Component (PointQuery l) PointInput (PointOutput l) Aff
component = H.mkComponent { initialState, eval, render }

initialState :: forall l. PointInput -> PointState l
initialState input =
  { point: input.point
  , statuses: Set.empty
  , mb_bufferInput: none
  }

eval :: forall l a. H.HalogenQ (PointQuery l) (PointAction l) PointInput a -> H.HalogenM (PointState l) (PointAction l) (PointSlots l) (PointOutput l) Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_PointAction
  , handleQuery = handleQuery
  , handleAction = handleAction
  , receive = pure <<< Receive_PointAction
  }

handleQuery :: forall l a. PointQuery l a -> PointM l (Maybe a)
handleQuery (ModifyStatuses_PointQuery f a) = do
  -- get >>= \{ statuses } -> Console.log $ "[Point] old statuses: " <> show statuses
  prop @"statuses" %= f
  -- get >>= \{ statuses } -> Console.log $ "[Point] new statuses: " <> show statuses
  gets _.statuses >>= \statuses -> do
    when (not $ statuses # Set.intersection ss_Focus # Set.isEmpty) do
      mb_elem_this <- H.getHTMLElementRef refLabel_point
      case mb_elem_this of
        Nothing -> pure unit
        Just elem_this -> liftEffect $ elem_this # HTMLElement.toElement # Element.scrollIntoView
  pure $ pure a
handleQuery (SetBufferInput_PointQuery mb_bufferInput a) = do
  modify_ _ { mb_bufferInput = mb_bufferInput }
  pure $ pure a
handleQuery (GetBufferInput_PointQuery k) = do
  state <- get
  pure $ pure $ k state.mb_bufferInput

ss_Focus = Set.fromFoldable [ Point_Handle_PointStatus, LeftFocus_PointStatus, RightFocus_PointStatus ]
ss_Left = Set.fromFoldable [ LeftFocus_PointStatus ] :: Set PointStatus
ss_Middle = Set.fromFoldable [ Point_Handle_PointStatus ] :: Set PointStatus
ss_Right = Set.fromFoldable [ RightFocus_PointStatus ] :: Set PointStatus

handleAction :: forall l. PointAction l -> PointM l Unit
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
handleAction (BufferOutput_PointAction bufferOutput) = do
  H.raise $ BufferOutput_PointOutput bufferOutput

render :: forall l. Show l => PointState l -> PointHTML l
render state =
  HH.div
    [ HP.ref refLabel_point
    , classes $ Array.fold
        [ [ "Point" ]
        , state.statuses # Set.toUnfoldable # map show
        , state.mb_bufferInput # foldMap (const [ "bufferIsOpen" ])
        ]
    , HE.onMouseDown MouseDown_PointAction
    , HE.onMouseEnter MouseEnter_PointAction
    ]
    case state.mb_bufferInput of
      Nothing -> [ HH.div [ classes [ "Left" ] ] [], HH.div [ classes [ "Middle" ] ] [], HH.div [ classes [ "Right" ] ] [] ]
      Just input -> case unit of
        _ | state.statuses # Set.subset ss_Left -> [ HH.div [ classes [ "Left" ] ] [ buffer ], HH.div [ classes [ "Middle" ] ] [], HH.div [ classes [ "Right" ] ] [] ]
        _ | state.statuses # Set.subset ss_Middle -> [ HH.div [ classes [ "Left" ] ] [], HH.div [ classes [ "Middle" ] ] [ buffer ], HH.div [ classes [ "Right" ] ] [] ]
        _ | state.statuses # Set.subset ss_Right -> [ HH.div [ classes [ "Left" ] ] [], HH.div [ classes [ "Middle" ] ] [], HH.div [ classes [ "Right" ] ] [ buffer ] ]
        _ -> [ HH.div [ classes [ "Left" ] ] [], HH.div [ classes [ "Middle" ] ] [], HH.div [ classes [ "Right" ] ] [] ]
        where
        buffer = HH.slot (Proxy @"Buffer") unit Buffer.component input BufferOutput_PointAction

refLabel_point = H.RefLabel "point"

