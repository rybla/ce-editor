-- | The simply typed lambda calculus with all inferred types.
module Theory.Stlc where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, get, modify, modify_, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Utility (fromMaybeM)

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
    FunTm x a -> "(" <> x <> " => " <> show a <> ")"
    AppTm f a -> "(" <> show f <> " " <> show a <> ")"

instance Eq Tm where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- type checking

type TcMT m = ExceptT String (StateT Env (WriterT (Array String) m))

type Env =
  { holeIndex :: Int
  , sigma :: Map Int Ty
  }

newtype Ctx = Ctx (Map String Ty)

derive instance Newtype Ctx _

instance Show Ctx where
  show (Ctx gamma) =
    gamma
      # Map.toUnfoldable
      # map (\(x /\ ty) -> show x <> ": " <> show ty)
      # Array.intercalate ", "
      # \s -> "[" <> s <> "]"

inferVar :: forall m. Monad m => Ctx -> String -> TcMT m Ty
inferVar (Ctx gamma) x = gamma # Map.lookup x # fromMaybeM do throwError $ "check: mal-scoped variable: " <> show x

setTyOfVar :: String -> Ty -> Ctx -> Ctx
setTyOfVar x ty = Newtype.over Ctx (Map.insert x ty)

runM :: forall m a. Monad m => TcMT m a -> m (((String \/ a) /\ Env) /\ Array String)
runM =
  runExceptT
    >>> flip runStateT { holeIndex: 0, sigma: Map.empty }
    >>> runWriterT

freshHoleTy :: forall m. Monad m => TcMT m Ty
freshHoleTy = do
  env <- modify \env -> env { holeIndex = env.holeIndex + 1 }
  pure (HoleTy env.holeIndex)

norm :: forall m. Monad m => Ty -> TcMT m Ty
norm IntTy = pure IntTy
norm BoolTy = pure BoolTy
norm (ArrTy a b) = ArrTy <$> norm a <*> norm b
norm (HoleTy n) = do
  env <- get
  case env.sigma # Map.lookup n of
    Nothing -> pure (HoleTy n)
    Just ty' -> norm ty'

unify :: forall m. Monad m => Ty -> Ty -> TcMT m Ty
unify ty1 ty2 = do
  tell [ "unify " <> show ty1 <> " " <> show ty2 ]
  ty1' <- norm ty1
  ty2' <- norm ty2
  unify' ty1' ty2'

mentionsHoleTy :: Int -> Ty -> Boolean
mentionsHoleTy _ IntTy = false
mentionsHoleTy _ BoolTy = false
mentionsHoleTy n (ArrTy dom cod) = mentionsHoleTy n dom || mentionsHoleTy n cod
mentionsHoleTy n (HoleTy m) = n == m

unifyHole :: forall m. Monad m => Int -> Ty -> TcMT m Ty
unifyHole n ty = do
  if ty # mentionsHoleTy n then
    throwError $ "cannot unity " <> show (HoleTy n) <> " with " <> show ty <> " due to cyclic dependency"
  else do
    env <- get
    modify_ _ { sigma = env.sigma # Map.insert n ty }
    pure ty

unify' :: forall m. Monad m => Ty -> Ty -> TcMT m Ty
unify' ty1 (HoleTy n) = unifyHole n ty1
unify' (HoleTy n) ty2 = unifyHole n ty2
unify' IntTy IntTy = pure IntTy
unify' BoolTy BoolTy = pure BoolTy
unify' (ArrTy a1 b1) (ArrTy a2 b2) = do
  a <- unify a1 a2
  b <- unify b1 b2
  pure (ArrTy a b)
unify' ty1 ty2 = do
  throwError $ "unify: " <> show ty1 <> " " <> show ty2

check :: forall m. Monad m => Ctx -> Ty -> Tm -> TcMT m Unit
check gamma ty tm = do
  tell [ "check " <> show gamma <> " " <> show ty <> " " <> show tm ]
  ty' <- norm ty
  check' gamma ty' tm

check' :: forall m. Monad m => Ctx -> Ty -> Tm -> TcMT m Unit
check' gamma (HoleTy n) tm = do
  ty <- infer gamma tm
  unify ty (HoleTy n) # void
check' gamma ty (VarTm x) = do
  ty' <- inferVar gamma x
  unify ty ty' # void
check' _gamma IntTy (IntTm _) = do
  pure unit
check' _gamma BoolTy (BoolTm _) = do
  pure unit
check' gamma (ArrTy dom cod) (FunTm x tm) = do
  check (gamma # setTyOfVar x dom) cod tm
check' gamma cod (AppTm f a) = do
  dom <- freshHoleTy
  check gamma (ArrTy dom cod) f
  check gamma dom a
check' gamma ty tm = do
  throwError $ "check: " <> show gamma <> " |- " <> show ty <> " of " <> show tm

infer :: forall m. Monad m => Ctx -> Tm -> TcMT m Ty
infer gamma tm = do
  tell [ "infer " <> show gamma <> " " <> show tm ]
  ty <- infer' gamma tm
  norm ty

infer' :: forall m. Monad m => Ctx -> Tm -> TcMT m Ty
infer' _gamma (IntTm _) = do
  pure IntTy
infer' _gamma (BoolTm _) = do
  pure BoolTy
infer' gamma (VarTm x) = inferVar gamma x
infer' gamma (FunTm x tm) = do
  dom <- freshHoleTy
  cod <- infer (gamma # setTyOfVar x dom) tm
  pure (ArrTy dom cod)
infer' gamma (AppTm f a) = do
  dom <- freshHoleTy
  cod <- freshHoleTy
  check gamma (ArrTy dom cod) f
  check gamma dom a
  pure cod

--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    example tm = do
      (err_or_ty /\ env) /\ logs <- runM $ infer (Ctx Map.empty) tm
      Console.log $ "----------------"
      Console.log $ "example: " <> show tm
      Console.log $ "----------------"
      Console.log $ "logs:"
      logs # traverse_ \log ->
        Console.log $ "  • " <> log
      Console.log $ "----------------"
      Console.log $ "result:"
      Console.log $ "  • env: " <> show env
      case err_or_ty of
        Left err -> Console.log $ "  • error: " <> err
        Right ty -> Console.log $ "  • type: " <> show ty

  traverse_ example
    [ FunTm "x" (VarTm "x" `AppTm` (VarTm "x"))
    ]
