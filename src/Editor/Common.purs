module Editor.Common where

import Data.Expr (Expr)

--------------------------------------------------------------------------------

type Editor =
  { name :: String
  , initial_expr :: Expr
  }
