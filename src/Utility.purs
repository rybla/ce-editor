module Utility where

import Prelude

import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Warn, Text)
import Type.Row.Homogeneous (class Homogeneous)

todo :: forall a. Warn (Text "contains TODOs") => String -> a
todo msg = unsafeCrashWith $ "[[TODO]]\n" <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[[BUG]]\n" <> msg

impossible :: forall @a. Unit -> a
impossible _ = bug "impossible"

replaceFormatVars :: Map String String -> String -> String
replaceFormatVars sigma = go (Map.toUnfoldable sigma)
  where
  go Nil s = s
  go ((k /\ v) : sigma') s = go sigma' $ String.replace (String.Pattern $ "{{" <> k <> "}}") (String.Replacement v) s

replaceFormatVars' ∷ forall r. Homogeneous r String ⇒ Record r → String → String
replaceFormatVars' sigma = replaceFormatVars (fromHomogeneousToMap sigma)

format = replaceFormatVars'

fromHomogeneousToMap :: forall r a. Homogeneous r a => Record r -> Map String a
fromHomogeneousToMap r = r
  # Object.fromHomogeneous
  # (Object.toUnfoldable :: _ -> List _)
  # Map.fromFoldable

