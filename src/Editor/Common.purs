module Editor.Common where

import Data.ExprNew (Expr, Handle)

--------------------------------------------------------------------------------

type Editor =
  { name :: String
  , initial_exprs :: Array Expr
  , initial_handle :: Handle
  }
