module Editor.Common where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.Array as Array
import Data.Expr (Edit(..), EditMenu, Expr, Fragment, Handle)
import Data.Expr.Edit as Expr.Edit
import Data.Foldable (fold)
import Data.Lazy as Lazy
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)
import Utility (todo)

--------------------------------------------------------------------------------

data Editor l = Editor
  { name :: String
  , initial_expr :: Expr l
  , initial_handle :: Handle
  , getEditMenu :: Expr l -> Handle -> EditMenu l
  , getShortcut :: Expr l -> Handle -> KeyInfo -> Maybe (Edit l)
  , isValidHandle :: Expr l -> Handle -> Boolean
  , assembleExpr :: AssembleExpr l
  }

type AssembleExpr l =
  forall w i
   . { label :: l
     , kids :: Array (RenderM (Array (HTML w i)))
     , points :: Array (HTML w i)
     }
  -> RenderM (Array (HTML w i))

type RenderM = Reader RenderCtx

runRenderM :: forall a. RenderM a -> a
runRenderM = flip runReader
  { indentLevel: -1
  }

type RenderCtx =
  { indentLevel :: Int
  }

--------------------------------------------------------------------------------

assembleExpr_default :: forall l. Show l => AssembleExpr l
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

-- TODO: this is an unnecessary layer
mkPasteFragmentEdit ∷ ∀ (l ∷ Type). Show l ⇒ Expr l → Handle → Fragment l → Edit l
mkPasteFragmentEdit root handle insertion = root # Expr.Edit.paste insertion handle

renderWarning msg = HH.div [ classes [ "Warning" ] ] [ HH.text msg ]