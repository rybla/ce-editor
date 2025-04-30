module Editor.Common where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (Reader, runReader)
import Data.Array as Array
import Data.Expr (Edit, EditMenu, Expr, Handle, M, PureEditorState)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

data Editor l = Editor
  { name :: String
  , initial_expr :: Expr l
  , initial_handle :: Handle
  , getEditMenu :: PureEditorState l -> EditMenu l
  , getShortcut :: KeyInfo -> PureEditorState l -> MaybeT M (Edit l)
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
  { indentLevel: 0 -- TODO: change this to 0 when allow feature of specifying which rendering constructs add to indentation level
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

renderWarning msg = HH.div [ classes [ "Warning" ] ] [ HH.text msg ]
