module Editor.Common where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, ask)
import Data.Array as Array
import Data.Diagnostic as Diagnostic
import Data.Expr (Edit, EditMenu, Expr, Handle, BasicEditorState)
import Data.Expr.Render (AssembleExpr)
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)
import Utility (todo)

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
      forall m r
       . Monad m
      => BasicEditorState (Label c r)
      -> GetEditM m c r (EditMenu m (Label c r))
  , getShortcut ::
      forall m r
       . Monad m
      => KeyInfo
      -> BasicEditorState (Label c r)
      -> GetEditM m c r (Maybe (Edit m (Label c r)))
  -- validity
  , isValidHandle :: forall r. Expr (Label c r) -> Handle -> Boolean
  -- processing
  , stampLabel :: Label c () -> Aff (StampedLabel c ())
  , assembleExpr :: AssembleExpr (StampedLabel c ())
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

type GetEditM m c r = ReaderT { stampLabel :: Label c () -> m (Label c r) } (Diagnostic.MT m)

stampTraversable
  :: forall m c r t
   . Monad m
  => Traversable t
  => t (Label c ())
  -> GetEditM m c r (t (Label c r))
stampTraversable t = do
  { stampLabel } <- ask
  t # traverse (stampLabel >>> lift >>> lift)

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

