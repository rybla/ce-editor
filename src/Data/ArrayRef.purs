module Data.ArrayRef (ArrayRef, new, length, pop, push) where

import Prelude

import Data.Maybe (Maybe)
import Data.Unfoldable (none)
import Effect (Effect)

foreign import data ArrayRef :: Type -> Type

foreign import new_ :: forall a. Effect (ArrayRef a)
new = new_

foreign import length :: forall a. ArrayRef a -> Effect Int

foreign import pop_ :: forall a. { pure :: a -> Maybe a, none :: Maybe a } -> ArrayRef a -> Effect (Maybe a)
pop = pop_ { pure, none }

foreign import push :: forall a. a -> ArrayRef a -> Effect Unit

foreign import freeze :: forall a. ArrayRef a -> Array a

