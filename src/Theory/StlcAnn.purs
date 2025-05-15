module Theory.StlcAnn where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, get, modify, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (null, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as Newtype
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Class.Console as Console
import Utility (fromMaybeM, unLines, (#.))

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

type Tm = Tm_ Unit
type Tm_checked = Tm_ Ann

getAnn_Tm :: Tm_checked -> Ann
getAnn_Tm (IntTm ann _) = ann
getAnn_Tm (BoolTm ann _) = ann
getAnn_Tm (VarTm ann _) = ann
getAnn_Tm (FunTm ann _ _) = ann
getAnn_Tm (AppTm ann _ _) = ann

modifyAnn_Tm :: (Ann -> Ann) -> Tm_checked -> Tm_checked
modifyAnn_Tm f_ann (IntTm ann i) = (IntTm (f_ann ann) i)
modifyAnn_Tm f_ann (BoolTm ann b) = (BoolTm (f_ann ann) b)
modifyAnn_Tm f_ann (VarTm ann x) = (VarTm (f_ann ann) x)
modifyAnn_Tm f_ann (FunTm ann x b) = (FunTm (f_ann ann) x b)
modifyAnn_Tm f_ann (AppTm ann f a) = (AppTm (f_ann ann) f a)

addError_Tm :: String -> Tm_checked -> Tm_checked
addError_Tm err = modifyAnn_Tm \(Ann ann) -> Ann ann { errs = ann.errs <> [ err ] }

getTy_Tm :: Tm_checked -> Ty
getTy_Tm = getAnn_Tm >>> \(Ann ann) -> ann.ty

newtype Ann = Ann
  { ty :: Ty
  , errs :: Array String
  }

derive instance Newtype Ann _

data Tm_ ann
  = IntTm ann Int
  | BoolTm ann Boolean
  | VarTm ann String
  | FunTm ann String (Tm_ ann)
  | AppTm ann (Tm_ ann) (Tm_ ann)

int = IntTm unit
bool = BoolTm unit
var = VarTm unit
fun = FunTm unit
app = AppTm unit
apps = Array.foldl app

derive instance Generic (Tm_ ann) _

instance Show Tm where
  show = case _ of
    IntTm _ n -> show n
    BoolTm _ b -> show b
    VarTm _ x -> x
    FunTm _ x a -> "(" <> x <> " => " <> show a <> ")"
    AppTm _ f a -> "(" <> show f <> " " <> show a <> ")"

showErrors :: Array String -> String
showErrors = Array.intercalate "; " >>> \s -> "{" <> s <> "}"

showErrorsPrefix (Ann ann) = (if null ann.errs then "" else showErrors ann.errs <> " ")

instance Show Tm_checked where
  show = case _ of
    IntTm ann n -> showErrorsPrefix ann <> show n
    BoolTm ann b -> showErrorsPrefix ann <> show b
    VarTm ann x -> showErrorsPrefix ann <> x
    FunTm ann x a -> showErrorsPrefix ann <> "(" <> x <> " : " <> str_dom <> " => " <> show a <> ")"
      where
      str_dom = case (ann # unwrap).ty of
        ArrTy dom _ -> show dom
        _ -> "!!"
    AppTm ann f a -> showErrorsPrefix ann <> "(" <> show f <> " " <> show a <> ")"

instance Eq ann => Eq (Tm_ ann) where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- type checking

type TcMT m = StateT Env (WriterT (Array String) m)
type TcMT_E m = ExceptT String (StateT Env (WriterT (Array String) m))

type Env =
  { holeIndex :: Int
  , sigma :: Map Int Ty
  }

runM :: forall m a. Monad m => TcMT m a -> m ((a /\ Env) /\ Array String)
runM =
  flip runStateT { holeIndex: 0, sigma: Map.empty }
    >>> runWriterT

newtype Ctx = Ctx (Map String Ty)

derive instance Newtype Ctx _

instance Show Ctx where
  show (Ctx gamma) =
    gamma
      # Map.toUnfoldable
      # map (\(x /\ ty) -> show x <> ": " <> show ty)
      # Array.intercalate ", "
      # \s -> "[" <> s <> "]"

inferVar :: forall m. Monad m => Ctx -> String -> TcMT_E m Ty
inferVar (Ctx gamma) x = gamma # Map.lookup x # fromMaybeM do throwError $ "check: mal-scoped variable: " <> show x

setTyOfVar :: String -> Ty -> Ctx -> Ctx
setTyOfVar x ty = Newtype.over Ctx (Map.insert x ty)

freshHoleTy :: forall m. Monad m => TcMT m Ty
freshHoleTy = do
  env <- modify \env -> env { holeIndex = env.holeIndex + 1 }
  pure (HoleTy env.holeIndex)

mentionsHoleTy :: Int -> Ty -> Boolean
mentionsHoleTy _ IntTy = false
mentionsHoleTy _ BoolTy = false
mentionsHoleTy n (ArrTy dom cod) = mentionsHoleTy n dom || mentionsHoleTy n cod
mentionsHoleTy n (HoleTy m) = n == m

unifyHole :: forall m. Monad m => Int -> Ty -> TcMT_E m Ty
unifyHole n ty = do
  if ty # mentionsHoleTy n then
    throwError $ "cannot unify " <> show (HoleTy n) <> " with " <> show ty <> " due to cyclic dependency"
  else do
    env <- get
    modify_ _ { sigma = env.sigma # Map.insert n ty }
    pure ty

normTy :: forall m. Monad m => Ty -> TcMT m Ty
normTy IntTy = pure IntTy
normTy BoolTy = pure BoolTy
normTy (ArrTy a b) = ArrTy <$> normTy a <*> normTy b
normTy (HoleTy n) = do
  env <- get
  case env.sigma # Map.lookup n of
    Nothing -> pure (HoleTy n)
    Just ty' -> normTy ty'

normTy_Ann :: forall m. Monad m => Ann -> TcMT m Ann
normTy_Ann (Ann ann) = do
  ty <- normTy ann.ty
  pure (Ann ann { ty = ty })

normTy_Tm :: forall m. Monad m => Tm_checked -> TcMT m Tm_checked
normTy_Tm (IntTm ann i) = IntTm <$> normTy_Ann ann <*> pure i
normTy_Tm (BoolTm ann b) = BoolTm <$> normTy_Ann ann <*> pure b
normTy_Tm (VarTm ann x) = VarTm <$> normTy_Ann ann <*> pure x
normTy_Tm (FunTm ann x b) = FunTm <$> normTy_Ann ann <*> pure x <*> normTy_Tm b
normTy_Tm (AppTm ann f a) = AppTm <$> normTy_Ann ann <*> normTy_Tm f <*> normTy_Tm a

unify :: forall m. Monad m => Ty -> Ty -> TcMT_E m Ty
unify ty1 ty2 = do
  tell [ "unify " <> show ty1 <> " " <> show ty2 ]
  ty1' <- normTy ty1 # lift
  ty2' <- normTy ty2 # lift
  unify' ty1' ty2'

unify' :: forall m. Monad m => Ty -> Ty -> TcMT_E m Ty
unify' ty1 (HoleTy n) = unifyHole n ty1
unify' (HoleTy n) ty2 = unifyHole n ty2
unify' IntTy IntTy = pure IntTy
unify' BoolTy BoolTy = pure BoolTy
unify' (ArrTy a1 b1) (ArrTy a2 b2) = do
  a <- unify a1 a2
  b <- unify b1 b2
  pure (ArrTy a b)
unify' ty1 ty2 = do
  throwError $ "cannot unify " <> show ty1 <> " with " <> show ty2

check :: forall m. Monad m => Ctx -> Ty -> Tm -> TcMT m Tm_checked
check gamma ty tm = do
  tell [ "check " <> show gamma <> " " <> show ty <> " " <> show tm ]
  tm' <- infer gamma tm
  unify (tm' # getTy_Tm) ty #. runExceptT >>= case _ of
    Left err -> pure (tm' # addError_Tm err)
    Right _ -> normTy_Tm tm'

infer :: forall m. Monad m => Ctx -> Tm -> TcMT m Tm_checked
infer gamma tm = do
  tell [ "infer " <> show gamma <> " " <> show tm ]
  tm' <- infer' gamma tm
  normTy_Tm tm'

infer' :: forall m. Monad m => Ctx -> Tm -> TcMT m Tm_checked

infer' _gamma (IntTm _ i) = do
  pure (IntTm (Ann { ty: IntTy, errs: none }) i)

infer' _gamma (BoolTm _ b) = do
  pure (BoolTm (Ann { ty: BoolTy, errs: none }) b)

infer' gamma (VarTm _ x) = do
  inferVar gamma x #. runExceptT >>= case _ of
    Left err -> do
      ty <- freshHoleTy
      pure (VarTm (Ann { ty, errs: pure err }) x)
    Right ty -> do
      pure (VarTm (Ann { ty, errs: none }) x)

infer' gamma (FunTm _ x tm) = do
  dom <- freshHoleTy
  tm' <- infer (gamma # setTyOfVar x dom) tm
  pure (FunTm (Ann { ty: ArrTy dom (tm' # getTy_Tm), errs: none }) x tm')

infer' gamma (AppTm _ f a) = do
  dom <- freshHoleTy
  cod <- freshHoleTy
  f' <- check gamma (ArrTy dom cod) f
  a' <- check gamma dom a
  pure (AppTm (Ann { ty: cod, errs: none }) f' a')

--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    example tm = do
      (ty /\ env) /\ logs <- runM $ infer (Ctx Map.empty) tm
      Console.log $ "--------------------------------"
      Console.log $ "example: " <> show tm
      Console.log $ "logs:"
      logs # traverse_ \log ->
        Console.log $ "  • " <> log
      Console.log $ "result:"
      Console.log $ unLines
        [ "  • env:"
        , "      { holeIndex: " <> show env.holeIndex
        , "      , sigma: " <> "{ " <> env.sigma #. Map.toUnfoldable #. map (\(n /\ ty') -> show n <> " := " <> show ty') #. Array.intercalate ", " <> " }"
        , "      }"
        ]
      -- "  • env: {\n" <>
      --   "      }"
      Console.log $ "  • type: " <> show ty

  traverse_ example
    [ fun "x" (var "x" `apps` [ var "x" ])
    , fun "x" (var "x")
    , (fun "x" (var "x")) `apps` [ int 1 ]
    ]

