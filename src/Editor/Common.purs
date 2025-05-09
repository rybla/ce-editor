module Editor.Common where

import Prelude

import Control.Monad.Writer (Writer)
import Data.Array as Array
import Data.Expr (BasicEditorState, Edit, EditM, EditMenu, Expr, Handle, EditCtx)
import Data.Expr.Render (AssembleExpr)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (ComponentHTML) as H
import Halogen.HTML as HH
import Record as Record
import Type.Prelude (Proxy(..))
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

newtype Label c r = Label (Record (BaseLabelRow c r))

instance Show c => Show (Label c r) where
  show (Label l) = show l.con

instance Eq c => Eq (Label c r) where
  eq (Label l1) (Label l2) = l1.con == l2.con

instance Ord c => Ord (Label c r) where
  compare (Label l1) (Label l2) = compare l1.con l2.con

mapLabel :: forall c r c' r'. (Record (BaseLabelRow c r) -> Record (BaseLabelRow c' r')) -> Label c r -> Label c' r'
mapLabel f (Label l) = Label $ f l

type BaseLabelRow (c :: Type) r =
  ( con :: c
  | r
  )

getCon :: forall c r. Label c r -> c
getCon (Label { con }) = con

type StampedLabel c r = Label c (StampedLabelRow r)

type StampedLabelRow r =
  ( id :: String
  | r
  )

getId :: forall c r. StampedLabel c r -> String
getId (Label { id }) = id

--------------------------------------------------------------------------------

data Editor c = Editor
  { name :: String
  -- initializing
  , initialExpr :: Expr (Label c ())
  , initialHandle :: Handle
  -- editing
  , getEditMenu ::
      forall m
       . Monad m
      => BasicEditorState (Label c ()) (StampedLabel c ())
      -> EditM m (Label c ()) (StampedLabel c ()) (EditMenu m (Label c ()) (StampedLabel c ()))
  , getShortcut ::
      forall m
       . Monad m
      => KeyInfo
      -> BasicEditorState (Label c ()) (StampedLabel c ())
      -> EditM m (Label c ()) (StampedLabel c ()) (Edit m (Label c ()) (StampedLabel c ()))
  -- validity
  , isValidHandle :: forall r. Expr (Label c r) -> Handle -> Boolean
  -- processing
  , stampLabel :: Label c () -> Aff (StampedLabel c ())
  , assembleExpr :: AssembleExpr (StampedLabel c ())
  -- diagnostics
  , getDiagnostics :: BasicEditorState (Label c ()) (StampedLabel c ()) -> Array Diagnostic
  -- printing
  , printExpr :: forall r. Expr (Label c r) -> String
  }

newtype ExistsEditor = ExistsEditor (forall r. ExistsEditorK r -> r)
type ExistsEditorK r = forall c. Show c => Editor c -> r

mkExistsEditor :: ExistsEditorK ExistsEditor
mkExistsEditor a = ExistsEditor \k -> k a

runExistsEditor :: forall r. ExistsEditorK r -> ExistsEditor -> r
runExistsEditor k1 (ExistsEditor k2) = k2 k1

--------------------------------------------------------------------------------

data Diagnostic = Diagnostic
  { title :: String
  , content :: DiagnosticsPanelHTML
  }

type DiagnosticsPanelHTML = H.ComponentHTML DiagnosticsPanelAction DiagnosticsPanelSlots Aff

data DiagnosticsPanelAction = Initialize_DiagnosticsPanelAction

type DiagnosticsPanelSlots :: Row Type
type DiagnosticsPanelSlots = ()

--------------------------------------------------------------------------------

toEditCtx :: forall m c. MonadAff m => Editor c -> EditCtx m (Label c ()) (StampedLabel c ())
toEditCtx (Editor editor) =
  { stampLabel: editor.stampLabel >>> liftAff
  , unstampLabel
  }

unstampLabel :: forall c. StampedLabel c () -> Label c ()
unstampLabel (Label l) = Label (l # Record.delete (Proxy @"id"))

--------------------------------------------------------------------------------

assembleStampedExpr_default :: forall c r. Show c => AssembleExpr (StampedLabel c r)
assembleStampedExpr_default { label: label@(Label l), kids, points } = do
  kidsAndPoints <- map fold $ Array.zip points kids # traverse \(point /\ m_kid) -> do
    kid <- m_kid
    pure $ [ point ] <> kid
  pure $ fold
    [ [ (l.id <> "_begin") /\ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text "(" ] ]
    , [ (l.id <> "_label") /\ HH.div [ classes [ "Token", "foreign" ] ] [ HH.text $ show label ] ]
    , kidsAndPoints
    , [ points # Array.last # fromMaybe ((l.id <> "_missingLastPoint") /\ renderWarning "missing last point") ]
    , [ (l.id <> "_end") /\ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text ")" ] ]
    ]

assembleExpr_default :: forall c r. Show c => AssembleExpr (Label c r)
assembleExpr_default { label, kids, points } = do
  kidsAndPoints <- map fold $ Array.zip points kids # traverse \(point /\ m_kid) -> do
    kid <- m_kid
    pure $ [ point ] <> kid
  pure $ fold
    [ [ "TODO" /\ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text "(" ] ]
    , [ "TODO" /\ HH.div [ classes [ "Token", "foreign" ] ] [ HH.text $ show label ] ]
    , kidsAndPoints
    -- , [ points # Array.last # fromMaybe (renderWarning "missing last point") ]
    , [ points # Array.last # fromMaybe ("TODO" /\ renderWarning "missing last point") ]
    , [ "TODO" /\ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text ")" ] ]
    ]

renderWarning msg = HH.div [ classes [ "Warning" ] ] [ HH.text msg ]

