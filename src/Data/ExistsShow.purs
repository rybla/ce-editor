module Data.ExistsShow where

import Prelude

newtype ExistsShow = ExistsShow (forall r. ExistsShowK r -> r)
type ExistsShowK r = forall a. Show a => a -> r

mkExistsShow :: ExistsShowK ExistsShow
mkExistsShow a = ExistsShow \k -> k a

runExistsShow :: forall r. ExistsShowK r -> ExistsShow -> r
runExistsShow k1 (ExistsShow k2) = k2 k1

