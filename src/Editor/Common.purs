module Editor.Common where

import Data.Expr (Expr, Fragment, Handle)
import Data.Maybe (Maybe)

--------------------------------------------------------------------------------

type Editor =
  { name :: String
  , initial_exprs :: Array Expr
  , initial_handle :: Handle
  , example_fragment :: String -> Maybe Fragment
  }
