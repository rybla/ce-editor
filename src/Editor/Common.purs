module Editor.Common where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.Array as Array
import Data.Expr (Edit, EditMenu, Expr, Handle, PureEditorState)
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

newtype Label c r = Label (Record (LabelRow c r))
type LabelRow c r = (con :: c | r) :: Row Type

instance Show c => Show (Label c r) where
  show (Label l) = show l.con

instance Eq c => Eq (Label c r) where
  eq (Label l1) (Label l2) = l1.con == l2.con

instance Ord c => Ord (Label c r) where
  compare (Label l1) (Label l2) = compare l1.con l2.con

getCon :: forall c r. Label c r -> c
getCon (Label { con }) = con

--------------------------------------------------------------------------------

type IdRow r = (id :: String | r)

--------------------------------------------------------------------------------

data Editor c = Editor
  { name :: String
  , initialExpr :: Expr (Label c ())
  , initialHandle :: Handle
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
  , isValidHandle :: forall r. Expr (Label c r) -> Handle -> Boolean
  , assembleExpr :: AssembleExpr c
  , printExpr :: forall r. Expr (Label c r) -> String
  , liftLabel :: Label c () -> Aff (Label c ())
  }

newtype ExistsEditor = ExistsEditor (forall r. ExistsEditorK r -> r)
type ExistsEditorK r = forall c. Show c => Editor c -> r

mkExistsEditor :: ExistsEditorK ExistsEditor
mkExistsEditor a = ExistsEditor \k -> k a

runExistsEditor :: forall r. ExistsEditorK r -> ExistsEditor -> r
runExistsEditor k1 (ExistsEditor k2) = k2 k1

type AssembleExpr c =
  forall w i r
   . { label :: Label c r
     , kids :: Array (RenderM (Array (HTML w i)))
     , points :: Array (HTML w i)
     }
  -> RenderM (Array (HTML w i))

type RenderM = Reader RenderCtx

runRenderM :: forall a. RenderM a -> a
runRenderM = flip runReader
  { indentLevel: 0 -- TODO: change this to 0 when allow feature of specifying which rendering constructs add to indentation level
  }

type RenderCtx =
  { indentLevel :: Int
  }

--------------------------------------------------------------------------------

assembleExpr_default :: forall c. Show c => AssembleExpr c
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

