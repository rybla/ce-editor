module Editor.Common where

import Prelude

import Data.Array as Array
import Data.Expr (Edit(..), EditMenu, Expr, Fragment, Handle)
import Data.Expr.Edit as Expr.Edit
import Data.Lazy as Lazy
import Data.Maybe (Maybe, fromMaybe)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Ui.Event (KeyInfo)
import Ui.Halogen (classes)

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

mkPasteFragmentEdit ∷ ∀ (l ∷ Type). Show l ⇒ Expr l → Handle → Fragment l → Edit l
mkPasteFragmentEdit root handle frag = Fragment_Edit frag $ Lazy.defer \_ -> Expr.Edit.paste frag handle root

