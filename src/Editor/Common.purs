module Editor.Common where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (Reader, runReader)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Expr (Edit, EditMenu, Expr, Handle, M, PureEditorState)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

type Label c r = { con :: c | r }
type Label' c = Label c (meta :: { id :: String })

-- | c is type of constructors
data Editor c = Editor
  { name :: String
  , initial_expr :: Expr (Label' c)
  , initial_handle :: Handle
  , getEditMenu :: PureEditorState (Label' c) -> EditMenu (Label' c)
  , getShortcut :: KeyInfo -> PureEditorState (Label' c) -> MaybeT M (Edit (Label' c))
  , isValidHandle :: Expr (Label' c) -> Handle -> Boolean
  , assembleExpr :: AssembleExpr c
  }

type AssembleExpr c =
  forall w i
   . { label :: Label' c
     , kids :: Array (RenderM (Array (HTML w i)))
     , points :: Array (HTML w i)
     }
  -> RenderM (Array (HTML w i))

--------------------------------------------------------------------------------

type RenderM = Reader RenderCtx

runRenderM :: forall a. RenderM a -> a
runRenderM = flip runReader
  { indentLevel: 0 -- TODO: change this to 0 when allow feature of specifying which rendering constructs add to indentation level
  }

type RenderCtx =
  { indentLevel :: Int
  }

--------------------------------------------------------------------------------

type ExprHTML w i = KeyHTML w i \/ KeyHTML w i
type KeyHTML w i = String /\ HTML w i

type RenderedExprInfo =
  { id :: String
  }

fromExprHTML :: forall w i. RenderedExprInfo -> ExprHTML w i -> KeyHTML w i
fromExprHTML info = either (lmap ((info.id <> "_") <> _)) identity

--------------------------------------------------------------------------------

assembleExpr_default :: forall c. Show c => AssembleExpr c
assembleExpr_default { label, kids, points } = do
  kidsAndPoints <- map fold $ Array.zip points kids # traverse \(point /\ m_kid) -> do
    kid <- m_kid
    pure $ [ point ] <> kid
  pure $ fold
    [ [ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text "(" ] ]
    , [ HH.div [ classes [ "Token", "keyword" ] ] [ HH.text $ show label ] ]
    , kidsAndPoints
    , [ points # Array.last # fromMaybe (renderWarning "missing last point") ]
    , [ HH.div [ classes [ "Token", "punctuation" ] ] [ HH.text ")" ] ]
    ]

renderWarning msg = HH.div [ classes [ "Warning" ] ] [ HH.text msg ]
