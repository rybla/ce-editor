module Editor.Common where

import Data.Expr (Expr, Fragment, Handle)
import Data.Maybe (Maybe)

--------------------------------------------------------------------------------

type Editor l =
  { name :: String
  , initial_expr :: Expr l
  , initial_handle :: Handle
  , example_fragment :: String -> Maybe (Fragment l)
  , max_history_length :: Int
  }
