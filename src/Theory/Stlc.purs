-- | The simply typed lambda calculus with all inferred types.
module Theory.Stlc where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.State (State, get, modify, modify_)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Utility (fromMaybeM, todo)

--------------------------------------------------------------------------------
-- data definitions

data Ty
  = IntTy
  | BoolTy
  | ArrTy Ty Ty
  | HoleTy Int

derive instance Generic Ty _

instance Show Ty where
  show = case _ of
    IntTy -> "Int"
    BoolTy -> "Bool"
    ArrTy a b -> "(" <> show a <> " -> " <> show b <> ")"
    HoleTy n -> "?" <> show n

instance Eq Ty where
  eq x = genericEq x

data Tm
  = IntTm Int
  | BoolTm Boolean
  | VarTm String
  | FunTm String Tm
  | AppTm Tm Tm

derive instance Generic Tm _

instance Show Tm where
  show = case _ of
    IntTm n -> show n
    BoolTm b -> show b
    VarTm x -> x
    FunTm x a -> "(" <> show x <> " => " <> show a <> ")"
    AppTm f a -> "(" <> show f <> " " <> show a <> ")"

instance Eq Tm where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- type checking

type M = ExceptT String (State Env)

type Env =
  { holeIndex :: Int
  , sigma :: Map Int Ty
  }

type Ctx = Map String Ty

freshHoleTy :: M Ty
freshHoleTy = do
  env <- modify \env -> env { holeIndex = env.holeIndex + 1 }
  pure (HoleTy env.holeIndex)

norm :: Ty -> M Ty
norm IntTy = pure IntTy
norm BoolTy = pure BoolTy
norm (ArrTy a b) = ArrTy <$> norm a <*> norm b
norm (HoleTy n) = do
  env <- get
  case env.sigma # Map.lookup n of
    Nothing -> pure (HoleTy n)
    Just ty' -> norm ty'

unify :: Ty -> Ty -> M Ty
unify ty1_ ty2_ = do
  ty1 <- norm ty1_
  ty2 <- norm ty2_
  unify' ty1 ty2

unify' :: Ty -> Ty -> M Ty
unify' ty1 (HoleTy n) = do
  env <- get
  modify_ _ { sigma = env.sigma # Map.insert n ty1 }
  pure ty1
unify' (HoleTy n) ty2 = do
  env <- get
  modify_ _ { sigma = env.sigma # Map.insert n ty2 }
  pure ty2
unify' IntTy IntTy = pure IntTy
unify' BoolTy BoolTy = pure BoolTy
unify' (ArrTy a1 b1) (ArrTy a2 b2) = do
  a <- unify a1 a2
  b <- unify b1 b2
  pure (ArrTy a b)
unify' ty1 ty2 = do
  throwError $ "unify: " <> show ty1 <> " ~ " <> show ty2

check :: Ctx -> Ty -> Tm -> M Unit
check gamma ty tm = do
  ty' <- norm ty
  check' gamma ty' tm

check' :: Ctx -> Ty -> Tm -> M Unit
check' gamma (HoleTy n) tm = do
  ty <- infer gamma tm
  unify ty (HoleTy n) # void
check' gamma ty (VarTm x) = do
  ty' <- gamma # Map.lookup x # fromMaybeM do throwError $ "check: mal-scoped variable: " <> show x
  unify ty ty' # void
check' _gamma IntTy (IntTm _) = pure unit
check' _gamma BoolTy (BoolTm _) = pure unit
check' gamma (ArrTy a b) (FunTm x tm) = do
  check (gamma # Map.insert x a) b tm
check' gamma ty (AppTm f a) = do
  ty' <- freshHoleTy
  check gamma (ArrTy ty' ty) f
  check gamma ty' a
check' gamma ty tm = do
  throwError $ "check: " <> show gamma <> " |- " <> show ty <> " of " <> show tm

-- TODO
infer :: Ctx -> Tm -> M Ty
infer gamma (IntTm _) = pure IntTy
infer gamma (BoolTm _) = pure BoolTy
infer gamma (VarTm x) = do
  gamma # Map.lookup x # fromMaybeM do throwError $ "check: mal-scoped variable: " <> show x
infer gamma (FunTm x tm) = do
  ty <- freshHoleTy
  infer (gamma # Map.insert x ty) tm
infer gamma (AppTm _ _) = todo ""

--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  pure unit

