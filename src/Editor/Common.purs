module Editor.Common where

import Data.Expr (BufferOptions, Expr, Handle)

--------------------------------------------------------------------------------

type Editor l =
  { name :: String
  , initial_expr :: Expr l
  , initial_handle :: Handle
  , bufferOptions :: Handle -> Expr l -> BufferOptions l
  , max_history_length :: Int
  }

