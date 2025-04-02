module Editor.Common where

import Data.Expr (Expr, Handle)

--------------------------------------------------------------------------------

type Editor =
  { name :: String
  , initial_exprs :: Array Expr
  , initial_handle :: Handle
  }
