module Editor.Common where

import Prelude

import Data.Array as Array
import Data.Expr (Edit, EditMenu, Expr, Handle, PureEditorState)
import Data.Expr.Render (AssembleExpr)
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

-- TODO make mappable?

newtype Label c r = Label (Record (BaseLabelRow c r))

instance Show c => Show (Label c r) where
  show (Label l) = show l.con

instance Eq c => Eq (Label c r) where
  eq (Label l1) (Label l2) = l1.con == l2.con

instance Ord c => Ord (Label c r) where
  compare (Label l1) (Label l2) = compare l1.con l2.con

type BaseLabelRow (c :: Type) r =
  ( con :: c
  | r
  )

getCon :: forall c r. Label c r -> c
getCon (Label { con }) = con

type AnnotatedLabel c r = Label c (AnnotatedLabelRow r)

type AnnotatedLabelRow r =
  ( id :: String
  | r
  )

getId :: forall c r. AnnotatedLabel c r -> String
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
      => PureEditorState (Label c r)
      -> EditMenu m (Label c ()) (Label c r)
  , getShortcut ::
      forall m r
       . Monad m
      => KeyInfo
      -> PureEditorState (Label c r)
      -> Maybe (Edit m (Label c ()) (Label c r))
  -- validity
  , isValidHandle :: forall r. Expr (Label c r) -> Handle -> Boolean
  -- rendering
  , annotateLabel :: Label c () -> Aff (AnnotatedLabel c ())
  , assembleAnnotatedExpr :: AssembleExpr (AnnotatedLabel c ())
  , assembleExpr :: AssembleExpr (Label c ())
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

assembleExpr_default :: forall c r. Show c => AssembleExpr (Label c r)
assembleExpr_default { label, kids, points } = do
  kidsAndPoints <- map fold $ Array.zip points kids # traverse \(point /\ m_kid) -> do
    kid <- m_kid
    pure $ [ point ] <> kid
  pure $ fold
    [ [ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text "(" ] ]
    , [ HH.div [ classes [ "Token", "foreign" ] ] [ HH.text $ show label ] ]
    , kidsAndPoints
    , [ points # Array.last # fromMaybe (renderWarning "missing last point") ]
    , [ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text ")" ] ]
    ]

renderWarning msg = HH.div [ classes [ "Warning" ] ] [ HH.text msg ]

