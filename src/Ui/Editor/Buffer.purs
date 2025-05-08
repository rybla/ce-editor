module Ui.Editor.Buffer where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!))
import Data.Const (Const(..))
import Data.Expr (EditInfo(..), Edit_(..), Point(..))
import Data.Expr.Render (RenderArgs, renderFragment)
import Data.Expr.Render as Expr.Render
import Data.Foldable (fold, length, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String.CodePoints as String.CodePoints
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Editor (Editor(..), StampedLabel, getId, toEditCtx)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Ui.Editor.Common (BufferAction(..), BufferHTML, BufferInput, BufferM, BufferOutput(..), BufferQuery, BufferState, BufferSlots)
import Ui.Editor.Config as Config
import Ui.Editor.Console.Messages as Console.Messages
import Ui.Event (fromEventToKeyInfo, matchKeyInfoPattern', matchMapKeyInfo) as Event
import Ui.Event (keyMember, not_alt, not_cmd)
import Ui.Halogen (classes)
import Utility (fromMaybeM)
import Web.Event.Event (preventDefault) as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window as HTML.Window
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEvent

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

component :: forall c. Show c => H.Component BufferQuery (BufferInput c) (BufferOutput c) Aff
component = H.mkComponent { initialState, eval, render }

--------------------------------------------------------------------------------
-- initialState
--------------------------------------------------------------------------------

initialState :: forall l. BufferInput l -> BufferState l
initialState input =
  { editor: input.editor
  , point: input.point
  , query: input.query
  , menu: input.menu
  , option_i: none
  , menu_queried: none
  }

--------------------------------------------------------------------------------
-- eval
--------------------------------------------------------------------------------

eval :: forall l a. H.HalogenQ BufferQuery (BufferAction l) (BufferInput l) a -> H.HalogenM (BufferState l) (BufferAction l) BufferSlots (BufferOutput l) Aff a
eval = H.mkEval H.defaultEval
  { initialize = pure Initialize_BufferAction
  , handleQuery = handleQuery
  , handleAction = handleAction
  }

--------------------------------------------------------------------------------
-- handleQuery
--------------------------------------------------------------------------------

handleQuery :: forall l a. BufferQuery a -> BufferM l (Maybe a)
handleQuery (Const x) = absurd x

--------------------------------------------------------------------------------
-- handleAction
--------------------------------------------------------------------------------

handleAction :: forall c. BufferAction c -> BufferM c Unit

handleAction Initialize_BufferAction = do
  when Config.log_initializations do
    liftEffect $ Console.Messages.push_message $ HH.text $ "[Buffer.initialize]"

  -- resizing input
  H.getHTMLElementRef refLabel_input >>= \mb_elem_input -> do
    elem_input <- mb_elem_input # fromMaybeM do liftEffect $ throw "[Buffer] input element doesn't exist"
    liftEffect $ elem_input # HTMLElement.focus
  doc <- liftEffect $ HTML.window >>= HTML.Window.document
  H.subscribe' \_subId -> HQE.eventListener KeyboardEvent.keydown (doc # HTMLDocument.toEventTarget) $ pure <<< KeyDown_BufferAction

  setQuery =<< resizeQueryInput

handleAction (Receive_BufferAction input) = do
  put $ initialState input
  setQuery =<< resizeQueryInput

handleAction (KeyDown_BufferAction event) = do
  let ki = event # Event.fromEventToKeyInfo
  state <- get
  case unit of
    _ | ki # Event.matchKeyInfoPattern' [ keyMember submitBuffer_keys, not_cmd, not_alt ] -> do
      liftEffect $ event # Event.preventDefault
      case state.option_i of
        Nothing -> pure unit
        Just i -> do
          _key /\ o <- state.menu_queried !! i # fromMaybeM do liftEffect $ throw "impossible for option_i to be out of bounds of menu_queried"
          H.raise $ SubmitBuffer_BufferOutput o
    _ | Just cd <- ki # Event.matchMapKeyInfo (unwrap >>> _.key >>> fromKeyToCycleDir) { cmd: pure false, shift: pure false, alt: pure false } -> do
      liftEffect $ event # Event.preventDefault
      case state.option_i /\ cd of
        Just i /\ Prev -> modify_ _ { option_i = pure $ (i - 1) `mod` (state.menu_queried # length) }
        Just i /\ Next -> modify_ _ { option_i = pure $ (i + 1) `mod` (state.menu_queried # length) }
        _ -> pure unit
    _ -> pure unit

handleAction (QueryInput_BufferAction _event) = do
  setQuery =<< resizeQueryInput

submitBuffer_keys = Set.fromFoldable [ "Tab", "Enter", " " ]

resizeQueryInput = do
  elem <- H.getHTMLElementRef refLabel_input >>= fromMaybeM do liftEffect $ throw "TODO"
  inputElem <- elem # HTMLInputElement.fromHTMLElement # fromMaybeM do liftEffect $ throw "TODO"
  let minSize = 1
  value <- liftEffect $ inputElem # HTMLInputElement.value
  liftEffect $ inputElem # HTMLInputElement.setSize (max minSize (value # String.CodePoints.length))
  pure value

--------------------------------------------------------------------------------
-- setQuery
--------------------------------------------------------------------------------

setQuery :: forall l. String -> BufferM l Unit
setQuery query = do
  state <- get
  mb_menu_queried <- state.menu query
    # flip runReaderT (state.editor # toEditCtx)
    # runMaybeT
    # lift
  case mb_menu_queried of
    Nothing -> pure unit
    Just menu_queried -> do
      modify_ _
        { query = query
        , menu_queried = menu_queried
        , option_i = if null menu_queried then none else pure 0
        }

--------------------------------------------------------------------------------
-- cycle menu
--------------------------------------------------------------------------------

data CycleDir = Prev | Next

fromKeyToCycleDir :: String -> Maybe CycleDir
fromKeyToCycleDir "ArrowUp" = pure Prev
fromKeyToCycleDir "ArrowDown" = pure Next
fromKeyToCycleDir _ = none

--------------------------------------------------------------------------------
-- render
--------------------------------------------------------------------------------

refLabel_input = H.RefLabel "input"

render :: forall c. Show c => BufferState c -> BufferHTML c
render state =
  HH.div [ classes $ fold [ [ "Buffer" ] ] ]
    [ HH.input
        [ classes [ "query" ]
        , HP.ref refLabel_input
        , HP.value state.query
        , HE.onInput QueryInput_BufferAction
        , HP.spellcheck false
        ]
    , HHK.div [ classes [ "menu" ] ]
        $ state.menu_queried # mapWithIndex \i (key /\ edit_) ->
            key /\ HH.div
              [ classes $ fold [ [ "Edit" ], if Just i /= state.option_i then [] else [ "selected" ] ] ]
              case edit_ of
                Edit { info: Insert_EditInfo info } ->
                  [ HHK.div [ classes [ "Expr" ] ] $
                      info.insertion
                        # renderFragment (renderArgs state.editor) (state.point # unwrap).path
                        # flip runReader
                            { indentLevel: 0
                            }
                  ]
                Edit { info: Remove_EditInfo _ } ->
                  [ HH.div [] [ HH.text "remove" ]
                  ]
                Edit { info: Copy_EditInfo _ } ->
                  [ HH.div [] [ HH.text "copy" ]
                  ]

    ]

renderArgs :: forall c w i. Show c => Editor c -> RenderArgs (StampedLabel c ()) w i
renderArgs (Editor editor) =
  { renderKid: renderExpr (Editor editor)
  , renderPoint: renderPoint (Editor editor)
  , assembleExpr: editor.assembleExpr
  }
  where
  renderExpr editor' path expr = Expr.Render.renderExpr (renderArgs editor') path expr

  renderPoint _ label (Point { j }) =
    ((label # getId) <> "_point_" <> show j) /\
      HH.div [ classes [ "Point" ] ] [ HH.text " " ]

