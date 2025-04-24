module Editor.Common where

import Prelude

import Data.Array as Array
import Data.Expr (BufferOption(..), BufferOptions, Expr, Fragment, Handle)
import Data.Expr.Edit as Expr.Edit
import Data.Lazy as Lazy
import Data.Maybe (fromMaybe)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Halogen (classes)

--------------------------------------------------------------------------------

data Editor l = Editor
  { name :: String
  , initial_expr :: Expr l
  , initial_handle :: Handle
  , bufferOptions :: Expr l -> Handle -> BufferOptions l
  , validHandle :: Expr l -> Handle -> Boolean
  , assembleExpr :: AssembleExpr l
  , historyLength_max :: Int
  }

type AssembleExpr l =
  forall w i
   . { label :: l
     , kids :: Array (HTML w i)
     , points :: Array (HTML w i)
     }
  -> Array (HTML w i)

--------------------------------------------------------------------------------

assembleExpr_default :: forall l. Show l => AssembleExpr l
assembleExpr_default { label, kids, points } = Array.fold
  [ [ HH.div [ classes [ "Punctuation" ] ] [ HH.text "(" ] ]
  , [ HH.div [ classes [ "label" ] ] [ HH.text $ show label ] ]
  , Array.fold $ Array.zipWith (\kid point -> [ point, kid ]) kids points
  , [ points # Array.last # fromMaybe (HH.div [] [ HH.text "{{missing last point}}" ]) ]
  , [ HH.div [ classes [ "Punctuation" ] ] [ HH.text ")" ] ]
  ]

mkPasteFragmentBufferOption ∷ ∀ (l ∷ Type). Show l ⇒ Expr l → Handle → Fragment l → BufferOption l
mkPasteFragmentBufferOption root handle frag = Fragment_BufferOption frag $ Lazy.defer \_ -> Expr.Edit.paste frag handle root

