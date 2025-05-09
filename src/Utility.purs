module Utility where

import Prelude

import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array.ST as STArray
import Data.Either (fromRight')
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Function (applyFlipped)
import Data.HeytingAlgebra (implies)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Warn, Text)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

todo :: forall a. Warn (Text "contains TODOs") => String -> a
todo msg = unsafeCrashWith $ "[[TODO]]\n" <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[[BUG]]\n" <> msg

assert :: String -> Boolean -> Unit
assert msg b = if b then unit else bug $ "failed assertion: " <> msg

impossible :: forall @a. String -> Unit -> a
impossible msg _ = bug $ "impossible: " <> msg

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

forget :: forall a b. a -> b -> b
forget _ b = b

extractAt_Array :: forall a. Int -> Array a -> Maybe { before :: Array a, here :: a, after :: Array a }
extractAt_Array i xs = do
  { before, after: after_ } <- splitAt_Array i xs
  { head: here, tail: after } <- Array.uncons after_
  pure { before, here, after }

splitAt_Array :: forall a. Int -> Array a -> Maybe { before :: Array a, after :: Array a }
splitAt_Array i xs | 0 <= i && i < Array.length xs = Just { before: Array.take i xs, after: Array.drop i xs }
splitAt_Array _ _ = Nothing

extractAt_List :: forall a. Int -> List a -> Maybe { before :: List a, here :: a, after :: List a }
extractAt_List i xs = do
  { before, after: after_ } <- splitAt_List i xs
  { head: here, tail: after } <- List.uncons after_
  pure { before, here, after }

splitAt_List :: forall a. Int -> List a -> Maybe { before :: List a, after :: List a }
splitAt_List i xs | 0 <= i && i < List.length xs = Just { before: List.take i xs, after: List.drop i xs }
splitAt_List _ _ = Nothing

fromMaybeM ∷ ∀ f a. Applicative f ⇒ f a → Maybe a → f a
fromMaybeM ma Nothing = ma
fromMaybeM _ (Just a) = pure a

insertSpanAt_Array :: forall a. Int -> Array a -> Array a -> Array a
insertSpanAt_Array i xs ys = split.before <> xs <> split.after
  where
  split = Array.splitAt i ys

extractSpan_Array :: forall a. Int -> Int -> Array a -> { before :: Array a, here :: Array a, after :: Array a }
extractSpan_Array i_L i_R xs =
  let
    { before: before_, after } = Array.splitAt i_R xs
    { before, after: here } = Array.splitAt i_L before_
  in
    { before, here, after }

isAlpha :: String -> Boolean
isAlpha = Regex.test isAlpha_regex

isAlpha_regex ∷ Regex
isAlpha_regex = Regex.regex "^[a-zA-Z]$" mempty # fromRight' (impossible "isAlpha_regex: failure")

infixr 2 implies as ==>

writeFlipped ∷ ∀ (a ∷ Type). Ref a → a → Effect Unit
writeFlipped = flip Ref.write

infix 4 writeFlipped as :=

modifyFlipped ∷ ∀ (a ∷ Type). Ref a → (a -> a) → Effect Unit
modifyFlipped r f = r # Ref.modify f # void

infix 4 modifyFlipped as :%=

lookup_unsafe :: forall a b. String -> a -> b
lookup_unsafe k a = a # unsafeCoerce # Object.lookup k # fromMaybe' \_ -> bug $ "Object doesn't have property " <> show k

infixr 1 applyFlipped as &
