module Ui.Editor.Id (freshId) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

freshId_counter ∷ Ref Int
freshId_counter = unsafePerformEffect $ Ref.new (-1)

freshId ∷ Effect String
freshId = freshId_counter
  # Ref.modify (_ + 1)
  # map show

