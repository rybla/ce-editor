module Utility where

import Prelude

import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array.ST as STArray
import Data.Foldable (foldl, foldr, or)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
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
  go ((k /\ v) : sigma') s = go sigma' $ String.replaceAll (String.Pattern $ "{{" <> k <> "}}") (String.Replacement v) s

replaceFormatVars' ∷ forall r. Homogeneous r String ⇒ Record r → String → String
replaceFormatVars' sigma = replaceFormatVars (fromHomogeneousToMap sigma)

format = replaceFormatVars'

fromHomogeneousToMap :: forall r a. Homogeneous r a => Record r -> Map String a
fromHomogeneousToMap r = r
  # Object.fromHomogeneous
  # (Object.toUnfoldable :: _ -> List _)
  # Map.fromFoldable

parens s = "(" <> s <> ")"
brackets s = "[" <> s <> "]"
braces s = "{" <> s <> "}"

spaces = Array.intercalate " "

allEqual xs = fromMaybe true do
  { head: x, tail: xs' } <- Array.uncons xs
  pure $ Array.all (_ == x) xs'

sortEquivalenceClasses :: forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmptyArray a)
sortEquivalenceClasses f xs = STArray.run do
  cs_ref <- STArray.new
  xs # traverse_ \x -> do
    added_ref <- STRef.new false
    cs <- cs_ref # STArray.freeze
    cs # traverseWithIndex_ \i c -> do
      let y = c # NEArray.head
      when (f x y) do
        cs_ref # STArray.modify i (NEArray.cons x) # void
        added_ref # STRef.modify (\_ -> true) # void
    whenM (not <$> (added_ref # STRef.read)) do
      cs_ref # STArray.push (NEArray.singleton x) # void
  pure cs_ref

-- const :: forall a b. a -> b -> a

forget :: forall a b. a -> b -> b
forget _ b = b
