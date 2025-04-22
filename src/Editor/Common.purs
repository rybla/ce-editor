module Editor.Common where

import Data.Expr (BufferOptions, Expr, Fragment, Handle, Point(..))
import Data.Maybe (Maybe)

--------------------------------------------------------------------------------

type Editor l =
  { name :: String
  , initial_expr :: Expr l
  , initial_handle :: Handle
  , example_fragment :: String -> Maybe (Fragment l)
  , bufferOptions_point :: Point -> BufferOptions l
  , max_history_length :: Int
  }

