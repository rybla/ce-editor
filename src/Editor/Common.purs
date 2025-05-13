module Editor.Common where

import Prelude

import Data.Array as Array
import Data.Expr (BasicEditorState, Edit, EditCtx, EditM, EditMenu, Expr, Handle, Point(..))
import Data.Expr.Render (Annotation, AssembleExpr)
import Data.Foldable (fold)
import Data.Id as Id
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML) as H
import Halogen (liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Record as Record
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

--------------------------------------------------------------------------------

type BaseLabelRow (c :: Type) r =
  ( con :: c
  | r
  )

getCon :: forall c r. Label c r -> c
getCon (Label { con }) = con

--------------------------------------------------------------------------------

type StampedLabel c r = Label c (StampedLabelRow r)

type StampedLabelRow r =
  ( id :: String
  | r
  )

getId :: forall c r. StampedLabel c r -> String
getId (Label { id }) = id

stampLabel
  :: forall c rA rB
   . (Record (BaseLabelRow c rA) -> Record (BaseLabelRow c rB))
  -> Label c rA
  -> Effect (StampedLabel c rB)
stampLabel f = \(Label l) -> do
  id <- Id.fresh # liftEffect
  pure $ Label $ Record.union { id } (f l)

--------------------------------------------------------------------------------

type AnnotatedLabel c r = StampedLabel c (AnnotatedLabelRow r)

type AnnotatedLabelRow r =
  ( ann :: Maybe (Array Annotation)
  | r
  )

annotateExpr_default :: forall c r. Expr (StampedLabel c r) -> Aff (Expr (AnnotatedLabel c r))
annotateExpr_default =
  traverse \(Label l) ->
    pure $ Label $ Record.union { ann: none } l

--------------------------------------------------------------------------------

-- TODO: when getting edits, should probably get an annotated expression so can
-- take types into account. this probably involves making Edit parametrized by
-- all 3 labels.

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
  , isHole :: forall r. Expr (Label c r) -> Point -> Boolean
  -- rendering
  , assembleStampedExpr :: AssembleExpr (StampedLabel c ())
  , assembleAnnotatedExpr :: AssembleExpr (AnnotatedLabel c ())
  -- diagnostics
  , getDiagnostics :: forall r1 r2. BasicEditorState (Label c r1) (AnnotatedLabel c r2) -> Array Diagnostic
  , annotateExpr :: forall r. Expr (StampedLabel c r) -> Aff (Expr (AnnotatedLabel c r))
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

mkEditCtx
  :: forall m c rA rB
   . MonadAff m
  => (Record (BaseLabelRow c rA) -> Record (BaseLabelRow c rB))
  -> (Record (BaseLabelRow c (StampedLabelRow rB)) -> Record (BaseLabelRow c rA))
  -> Editor c
  -> EditCtx m (Label c rA) (StampedLabel c rB)
mkEditCtx f g (Editor _editor) =
  { stampLabel: stampLabel f >>> liftEffect
  , unstampLabel: \(Label l) -> Label $ g l
  }

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

assembleExpr_default :: forall c r. Show c => String -> AssembleExpr (Label c r)
assembleExpr_default id { label, kids, points } = do
  kidsAndPoints <- map fold $ Array.zip points kids # traverse \(point /\ m_kid) -> do
    kid <- m_kid
    pure $ [ point ] <> kid
  pure $ fold
    [ [ (id <> "_begin") /\ HH.div [ HP.id (id <> "_begin"), classes [ "Token", "punctuation" ] ] [ HH.text "(" ] ]
    , [ (id <> "_label") /\ HH.div [ HP.id (id <> "_label"), classes [ "Token", "foreign" ] ] [ HH.text $ show label ] ]
    , kidsAndPoints
    , [ points # Array.last # fromMaybe ((id <> "_missingLastPoint") /\ renderWarning "missing last point") ]
    , [ (id <> "_end") /\ HH.div [ HP.id (id <> "_end"), classes [ "Token", "punctuation" ] ] [ HH.text ")" ] ]
    ]

renderWarning msg = HH.div [ classes [ "Warning" ] ] [ HH.text msg ]

