module Editor.Common where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (Reader, runReader)
import Data.Array as Array
import Data.Diagnostic as Diagnostic
import Data.Expr (Edit, EditMenu, Expr, Handle, PureEditorState)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

type Label c r = Record (con :: c | r)

data Editor c = Editor
  { name :: String
  , initialExpr :: Expr (Label c ())
  , initialHandle :: Handle
  , getEditMenu :: PureEditorState (Label c ()) -> EditMenu (Label c ())
  , getShortcut :: KeyInfo -> PureEditorState (Label c ()) -> MaybeT Diagnostic.M (Edit (Label c ()))
  , isValidHandle :: Expr (Label c ()) -> Handle -> Boolean
  , assembleExpr :: AssembleExpr c
  , printExpr :: Expr (Label c ()) -> String
  }

newtype ExistsEditor = ExistsEditor (forall r. ExistsEditorK r -> r)
type ExistsEditorK r = forall c. Show c => Editor c -> r

mkExistsEditor :: ExistsEditorK ExistsEditor
mkExistsEditor a = ExistsEditor \k -> k a

runExistsEditor :: forall r. ExistsEditorK r -> ExistsEditor -> r
runExistsEditor k1 (ExistsEditor k2) = k2 k1

type AssembleExpr c =
  forall w i
   . { label :: Label c ()
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
    , [ HH.div [ classes [ "Token", "foreign" ] ] [ HH.text $ show label.con ] ]
    , kidsAndPoints
    , [ points # Array.last # fromMaybe (renderWarning "missing last point") ]
    , [ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text ")" ] ]
    ]

renderWarning msg = HH.div [ classes [ "Warning" ] ] [ HH.text msg ]

