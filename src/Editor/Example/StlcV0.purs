module Editor.Example.StlcV0 where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ask, local, runReader)
import Control.Monad.State (StateT, get, modify, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr (BasicEditorState, Expr(..), Fragment(..), Handle(..), Index(..), Path, Point(..), Span(..), Step(..), atPoint, atSpan, atSubExpr, fromPathToString, fromPointToString, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth, stampTraversable)
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Render (AssembleExpr, KeyHTML, RenderArgs, RenderKid, RenderM)
import Data.Expr.Render as Expr.Render
import Data.Foldable (and, fold, foldMap, foldl, length, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Newtype as Newtype
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Unfoldable as Unfoldable
import Editor.Common (AnnotatedLabel, Diagnostic(..), Editor(..), Label(..), StampedLabel, assembleExpr_default, getCon, mapLabel)
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML, fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Properties as HP
import Record as Record
import Type.Proxy (Proxy(..))
import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)
import Utility (flatten, fromMaybeM, isIdentifier, parseBoolean, parseInt, unWords, (#.))

data Annotation
  = Info_Annotation PlainHTML
  | Error_Annotation PlainHTML

--------------------------------------------------------------------------------

newtype C = C String

derive instance Newtype C _

instance Show C where
  show (C s) = s

derive newtype instance Eq C

derive newtype instance Ord C

mkExprC c es = mkExpr (Label { con: c }) es

infix 0 mkExprC as %

mkToothC c es = mkTooth (Label { con: c }) es

infix 0 mkToothC as %<

mkSpanToothC c es = mkSpanTooth (Label { con: c }) es

infix 0 mkSpanToothC as %<*

--------------------------------------------------------------------------------

editor :: Editor C Ann
editor = Editor
  { name: "simply typed lambda calculus (v0: hole is ? term)"
  , initialExpr: C "Root" % []
  , initialHandle: Point_Handle $ Point { path: mempty, j: wrap 0 }
  , getEditMenu
  , getShortcut
  , isValidHandle
  , isHole
  , assembleStampedExpr
  , assembleAnnotatedExpr
  , printExpr
  , getDiagnostics
  , annotateExpr
  }

--------------------------------------------------------------------------------
-- getEditMenu
--------------------------------------------------------------------------------

getEditMenu state = do
  -- Lam
  zipper_Lam_params' <- zipper_Lam_params # stampTraversable
  edit_Lam_params <- Tuple "Lam_params" <$> Expr.Edit.insert (Zipper_Fragment zipper_Lam_params') state
  zipper_Lam_body' <- zipper_Lam_body # stampTraversable
  edit_Lam_body <- Tuple "Lam_body" <$> Expr.Edit.insert (Zipper_Fragment zipper_Lam_body') state
  -- App
  zipper_App' <- zipper_App # stampTraversable
  edit_App <- Tuple "App_func" <$> Expr.Edit.insert (Zipper_Fragment zipper_App') state
  -- Let
  zipper_Let_param' <- zipper_Let_param # stampTraversable
  edit_Let_param <- Tuple "Let_param" <$> Expr.Edit.insert (Zipper_Fragment zipper_Let_param') state
  zipper_Let_impl' <- zipper_Let_impl # stampTraversable
  edit_Let_impl <- Tuple "Let_impl" <$> Expr.Edit.insert (Zipper_Fragment zipper_Let_impl') state
  zipper_Let_body' <- zipper_Let_body # stampTraversable
  edit_Let_body <- Tuple "Let_body" <$> Expr.Edit.insert (Zipper_Fragment zipper_Let_body') state
  -- Hole
  expr_Hole' <- expr_Hole # stampTraversable
  edit_Hole <- Tuple "Hole" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Hole' ])) state
  pure \query -> do
    case query of
      "fun" -> pure [ edit_Lam_params, edit_Lam_body ]
      "app" -> pure [ edit_App ]
      "let" -> pure [ edit_Let_param, edit_Let_impl, edit_Let_body ]
      "?" -> pure [ edit_Hole ]
      -- Int
      _ | Just i <- query # parseInt -> do
        expr_Int' <- expr_Int i # stampTraversable
        edit_Int <- Tuple "Int" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Int' ])) state
        pure [ edit_Int ]
      -- Bool
      _ | Just b <- query # parseBoolean -> do
        expr_Boolean' <- expr_Boolean b # stampTraversable
        edit_Bool <- Tuple "Boolean" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Boolean' ])) state
        pure [ edit_Bool ]
      -- Var
      _ | query # isIdentifier -> do
        expr_Var' <- expr_Var query # stampTraversable
        edit_Var <- Tuple "Var" <$> Expr.Edit.insert (Span_Fragment (Span [ expr_Var' ])) state
        pure [ edit_Var ]
      _ -> pure []

--------------------------------------------------------------------------------
-- getShortcut
--------------------------------------------------------------------------------

getShortcut ki state
  | ki # matchKeyInfoPattern' [ keyEq "Enter", not_cmd, not_alt ] = do
      expr_LineBreak' <- expr_LineBreak # stampTraversable
      Expr.Edit.insert (Span_Fragment (Span [ expr_LineBreak' ])) state
  | ki # matchKeyInfoPattern' [ keyEq "(", not_cmd, not_alt ] = do
      zipper_App' <- zipper_App # stampTraversable
      Expr.Edit.insert (Zipper_Fragment zipper_App') state
  | otherwise = empty

--------------------------------------------------------------------------------
-- printExpr
--------------------------------------------------------------------------------

printExpr = go
  where
  go = case _ of
    Expr { l: Label { con: C "Root" }, kids } -> kids # map go # unWords
    Expr { l: Label { con: C "Var" }, kids: [ Expr { l: Label { con: C x } } ] } -> x
    Expr { l: Label { con: C "Let" }, kids: [ param, impl, body ] } -> "(let " <> param #. go <> " = " <> impl #. go <> " in " <> body #. go <> ")"
    Expr { l: Label { con: C "Let_param" }, kids } -> kids # map go # unWords
    Expr { l: Label { con: C "Let_impl" }, kids } -> kids # map go # unWords
    Expr { l: Label { con: C "Let_body" }, kids } -> kids # map go # unWords
    Expr { l: Label { con: C "Lam" }, kids: [ params, body ] } -> "(fun " <> params #. go <> " ⇒ " <> body #. go <> ")"
    Expr { l: Label { con: C "Lam_params" }, kids } -> kids # map go # unWords
    Expr { l: Label { con: C "Lam_body" }, kids } -> kids # map go # unWords
    Expr { l: Label { con: C "App" }, kids } -> "(" <> kids #. map go #. unWords <> ")"
    Expr { l: Label { con: C "LineBreak" }, kids: [] } -> "\n"
    Expr { l: Label { con: C "Hole" }, kids: [] } -> "_"
    e -> show e

--------------------------------------------------------------------------------
-- getDiagnostics
--------------------------------------------------------------------------------

getDiagnostics :: forall rA rB. BasicEditorState (Label C rA) (AnnotatedLabel C Ann rB) -> Array Diagnostic
getDiagnostics state = flatten @Array @Maybe
  [ state.clipboard <#> \frag ->
      Diagnostic
        { title: "Clipboard"
        , content:
            HHK.div [ classes [ "Expr" ] ] $
              frag
                # Expr.Render.renderFragment (renderArgs assembleExpr) none
                # flip runReader
                    { indentLevel: 0
                    }
        }
  , state.mb_handle >>= case _ of
      SpanH_Handle spanH _ -> do
        case atSpan spanH state.root of
          { here: Span [ (Expr { l: Label { ann } }) ] } | Just (Ann { mb_ty: Just ty }) <- ann -> pure $
            Diagnostic
              { title: "Type"
              , content: HHK.div [ classes [ "Type" ] ] [ "type" /\ HH.text (show ty) ]
              }
          _ -> none
      _ -> none

  ]
  where
  renderArgs :: forall r w i. AssembleExpr (Label C r) -> RenderArgs (Label C r) w i
  renderArgs assembleExpr' =
    { renderKid
    , renderPoint
    , assembleExpr: assembleExpr'
    }
    where
    renderKid path expr = Expr.Render.renderExpr (renderArgs assembleExpr') path expr

    renderPoint _label p =
      fromPointToString p /\
        HH.div [ classes [ "Point" ], HP.id (fromPointToString p) ]
          [ HH.text " " ]

--------------------------------------------------------------------------------
-- annotateExpr
--------------------------------------------------------------------------------

newtype Ann = Ann
  { annotations :: Array Annotation
  , mb_ty :: Maybe Ty
  }

derive instance Newtype Ann _

instance Show Ann where
  show (Ann ann) = "{ annotations: Array[" <> show (ann.annotations # Array.length) <> "], mb_ty: " <> show ann.mb_ty <> " }"

data Ty
  = IntTy
  | BoolTy
  | ArrTy Ty Ty
  | HoleTy Int

derive instance Generic Ty _

instance Show Ty where
  show = case _ of
    IntTy -> "Int"
    BoolTy -> "Boolean"
    ArrTy a b -> "(" <> show a <> " → " <> show b <> ")"
    HoleTy n -> "?" <> show n

type Tm r = Expr (StampedLabel C r)
type Tm_checked r = Expr (AnnotatedLabel C Ann r)

type TcMT m = StateT Env m
type TcMT_E m = ExceptT (Array Annotation) (StateT Env m)

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

getAnn :: forall m r. Monad m => Tm_checked r -> TcMT_E m Ann
getAnn (Expr { l: Label l }) = l.ann # fromMaybeM do throwError [ Error_Annotation $ HH.text "no annotation" ]

addAnnotations :: forall r. Array Annotation -> Tm_checked r -> Tm_checked r
addAnnotations as (Expr e@{ l: Label l }) = Expr e
  { l = Label l
      { ann =
          case l.ann of
            Nothing -> pure (Ann { annotations: as, mb_ty: none })
            Just (Ann ann) -> pure (Ann { annotations: ann.annotations <> as, mb_ty: ann.mb_ty })
      }
  }

getTy :: forall m r. Monad m => Tm_checked r -> TcMT_E m Ty
getTy t = do
  Ann ann <- t # getAnn
  ann.mb_ty # fromMaybeM do throwError [ Error_Annotation $ HH.text "no type annotation" ]

runTcMT :: forall m a. Monad m => TcMT m a -> m (a /\ Env)
runTcMT = flip runStateT
  { holeIndex: 0
  , sigma: Map.empty
  }

inferVar :: forall m. Monad m => Ctx -> String -> TcMT_E m Ty
inferVar (Ctx gamma) x = gamma # Map.lookup x # fromMaybeM do throwError [ Error_Annotation $ HH.text $ "variable not in scope: " <> x ]

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
  when (ty # mentionsHoleTy n) do
    throwError [ Error_Annotation $ HH.text $ "cannot unify " <> show (HoleTy n) <> " with " <> show ty <> " due to cyclic dependency" ]
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
  case ann.mb_ty of
    Nothing -> pure (Ann ann)
    Just ty -> do
      ty' <- normTy ty
      pure (Ann ann { mb_ty = pure ty' })

normTy_Tm :: forall m r. Monad m => Tm_checked r -> TcMT m (Tm_checked r)
normTy_Tm (Expr e@{ l: Label l }) = do
  case l.ann of
    Nothing -> pure (Expr e)
    Just ann -> do
      ann' <- normTy_Ann ann
      pure (Expr e { l = Label l { ann = pure ann' } })

unify :: forall m. Monad m => Ty -> Ty -> TcMT_E m Ty
unify ty1 ty2 = do
  -- tell [ "unify " <> show ty1 <> " " <> show ty2 ]
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
  throwError [ Error_Annotation $ HH.text $ "cannot unify " <> show ty1 <> " with " <> show ty2 ]

check :: forall m r. Monad m => Ctx -> Ty -> Tm r -> TcMT m (Tm_checked r)
check gamma ty tm = do
  -- tell [ "check " <> show gamma <> " " <> show ty <> " " <> show tm ]
  tm' <- infer gamma tm
  (unify ty =<< tm' #. getTy) #. runExceptT >>= case _ of
    Left anns -> pure (tm' # addAnnotations anns)
    Right _ -> normTy_Tm tm'

-- TODO: sensible feedback when wrong number of kids, rather than "foregin"
infer :: forall m r. Monad m => Ctx -> Tm r -> TcMT m (Tm_checked r)
infer gamma tm = do
  -- tell [ "infer " <> show gamma <> " " <> show tm ]
  tm' <- infer' gamma tm
  normTy_Tm tm'

infer' :: forall m r. Monad m => Ctx -> Tm r -> TcMT m (Tm_checked r)

infer' gamma (Expr { l: Label l@{ con: C "Root" }, kids }) = do
  kids' <- kids # traverse (infer' gamma)
  pure $ Expr { l: Label (l # Record.union { ann: none }), kids: kids' }

infer' _gamma (Expr { l: Label l@{ con: C "LineBreak" }, kids: [] }) = do
  pure $ Expr { l: Label (l # Record.union { ann: none }), kids: [] }

infer' _gamma (Expr { l: Label l@{ con: C "Hole" }, kids: [] }) = do
  ty <- freshHoleTy
  pure $ Expr { l: Label (l # Record.union { ann: pure $ Ann { mb_ty: pure ty, annotations: [] } }), kids: [] }

infer' _gamma (Expr { l: Label l@{ con: C "Int" }, kids }) = do
  pure $ Expr { l: Label (l # Record.union { ann: pure $ Ann { mb_ty: pure IntTy, annotations: [] } }), kids: kids # map (map (mapLabel (Record.union { ann: none }))) }

infer' _gamma (Expr { l: Label l@{ con: C "Boolean" }, kids }) = do
  pure $ Expr { l: Label (l # Record.union { ann: pure $ Ann { mb_ty: pure BoolTy, annotations: [] } }), kids: kids # map (map (mapLabel (Record.union { ann: none }))) }

infer' gamma (Expr { l: Label l@{ con: C "Var" }, kids: kids@[ Expr { l: Label { con: C x } } ] }) = do
  inferVar gamma x #. runExceptT >>= case _ of
    Left annotations -> do
      pure $ Expr { l: Label (l # Record.union { ann: pure $ Ann { mb_ty: none, annotations } }), kids: kids # map (map (mapLabel (Record.union { ann: none }))) }
    Right ty -> do
      pure $ Expr { l: Label (l # Record.union { ann: pure $ Ann { mb_ty: pure ty, annotations: [] } }), kids: kids # map (map (mapLabel (Record.union { ann: none }))) }

infer'
  gamma
  ( Expr
      { l: Label l@{ con: C "Lam" }
      , kids:
          [ Expr { l: Label l_params@{ con: C "Lam_params" }, kids: xs }
          , Expr { l: Label l_body@{ con: C "Lam_body" }, kids: bs }
          ]
      }
  ) = do
  -- check xs
  xs' :: Array (Expr (AnnotatedLabel C Ann r)) <- xs # traverse case _ of
    Expr { l: Label l_var@{ con: C "Var" }, kids: [ Expr { l: Label l_x@{ con: C _x }, kids: [] } ] } -> do
      pure $ Expr { l: Label (l_var # Record.union { ann: none }), kids: [ Expr { l: Label (l_x # Record.union { ann: none }), kids: [] } ] }
    Expr { l: Label l_foreign, kids } -> do
      pure $ Expr { l: Label (l_foreign # Record.union { ann: pure (Ann { mb_ty: none, annotations: [ Error_Annotation $ HH.text "non-Var in a parameter position" ] }) }), kids: kids # map (map (mapLabel (Record.union { ann: none }))) }
  -- extract params
  params <- xs'
    #
      ( traverse case _ of
          Expr { l: Label { con: C "Var" }, kids: [ Expr { l: Label { con: C x } } ] } -> freshHoleTy <#> \ty -> [ (x /\ ty) ]
          _ -> pure []
      )
    # map fold
  -- define context for b
  let gamma_b = params # foldl (\gamma' (x /\ ty) -> setTyOfVar x ty gamma') gamma
  -- check bs
  let
    mk mb_ty annotations bs' = pure $
      Expr
        { l: Label (l # Record.union { ann: pure (Ann { mb_ty, annotations }) })
        , kids:
            [ Expr { l: Label (l_params # Record.union { ann: none }), kids: xs' }
            , Expr { l: Label (l_body # Record.union { ann: none }), kids: bs' }
            ]
        }
  case bs of
    [] -> do
      ty <- freshHoleTy
      mk (pure ty) [ Error_Annotation $ HH.text "missing body" ] []
    [ b ] -> do
      b' <- b # infer gamma_b
      b' #. getTy #. runExceptT >>= case _ of
        -- TODO: I COULD put partial info here in the annotation since we DO know what dom is at this point
        Left annotations -> mk none annotations [ b' ]
        Right cod -> mk (pure (params # foldl (\cod' (_ /\ dom) -> ArrTy dom cod') cod)) none [ b' ]
    _ -> do
      bs' <- bs # traverse (infer gamma_b)
      mk none [ Error_Annotation $ HH.text $ "excessive bodies" ] bs'

-- handle multple args
infer'
  gamma
  ( Expr
      { l: Label l@{ con: C "App" }
      , kids -- [ f, a ]
      }
  ) = do
  case kids # Array.uncons of
    Nothing -> pure $ Expr { l: Label (l # Record.union { ann: pure (Ann { mb_ty: none, annotations: [ Error_Annotation $ HH.text "missing function to apply" ] }) }), kids: [] }
    Just { head: f, tail: as0 } -> do
      f' <- infer gamma f
      let
        go :: Maybe Ty -> Array (Expr _) -> TcMT m (Maybe Ty /\ (Array (Expr _)))
        go mb_phi as = case mb_phi /\ as #. Array.uncons of
          _ /\ Nothing -> pure $ mb_phi /\ []
          Nothing /\ Just { head: a, tail: as' } -> do
            a' <- infer gamma a
            _ /\ as'' <- go none as'
            pure $ none /\ Array.cons a' as''
          Just phi /\ Just { head: a, tail: as' } -> do
            dom <- freshHoleTy
            cod <- freshHoleTy
            unify phi (ArrTy dom cod) #. runExceptT >>= case _ of
              Left _annotations -> do
                a' <- infer gamma a
                _ /\ as'' <- go none as'
                pure $ none /\ Array.cons a' as''
              Right phi' -> do
                dom' <- normTy dom
                cod' <- normTy cod
                a' <- check gamma dom' a
                mb_phi' /\ as'' <- go (pure cod') as'
                pure $ mb_phi' /\ Array.cons a' as''
      getTy f' #. runExceptT >>= case _ of
        Left annotations -> do
          mb_ty /\ as' <- go none as0
          pure $ Expr
            { l: Label (l # Record.union { ann: pure (Ann { mb_ty, annotations: annotations }) })
            , kids: [ f' ] <> as'
            }
        Right phi -> do
          mb_ty /\ as' <- go (pure phi) as0
          pure $ Expr
            { l: Label (l # Record.union { ann: pure (Ann { mb_ty, annotations: if isNothing mb_ty then [ Error_Annotation $ HH.text $ "attempting to apply a non-function" ] else [] }) })
            , kids: [ f' ] <> as'
            }

-- TODO: C: "Let"

infer' _ (Expr { l: Label l, kids }) = do
  pure $ Expr { l: Label (l # Record.union { ann: pure (Ann { mb_ty: none, annotations: [ Info_Annotation $ HH.text $ "foreign constructor: " <> show l.con ] }) }), kids: kids # map (map (mapLabel (Record.union { ann: none }))) }

--------------------------------------------------------------------------------

annotateExpr :: forall r. Expr (StampedLabel C r) -> Aff (Expr (AnnotatedLabel C Ann r))
annotateExpr e = do
  e' /\ _env <- runTcMT do
    e' <- infer (Ctx Map.empty) e
    e' # traverse
      ( \(Label l) -> do
          mb_ann <- l.ann # traverse normTy_Ann
          pure $ Label (l # Record.set (Proxy @"ann") mb_ann)
      )
  -- normTy_Tm e'
  let
    e'' = e'
      # map
          ( Newtype.over Label \l -> l
              { ann = l.ann # map \(Ann ann) -> fromMaybe (Ann ann) do
                  ty <- ann.mb_ty
                  let annotations_new = ann.annotations <> [ Info_Annotation $ HH.text $ "type: " <> show ty ]
                  pure $ Ann ann { annotations = annotations_new }
              }
          )
  pure e''

--------------------------------------------------------------------------------
-- RenderKid predicates
--------------------------------------------------------------------------------

isntFormatting_RenderKid :: forall r w i. RenderKid (Label C r) w i -> Boolean
isntFormatting_RenderKid = fst >>> maybe true \(Label l) -> l.con /= C "LineBreak"

--------------------------------------------------------------------------------
-- assembly
--------------------------------------------------------------------------------

assembleAnnotatedExpr :: forall r. AssembleExpr (AnnotatedLabel C Ann r)
assembleAnnotatedExpr = assembleExpr_helper
  { getId: \_path (Label l) -> l.id
  , getAnnotations: \(Label l) -> l.ann
  }

assembleStampedExpr :: forall r. AssembleExpr (StampedLabel C r)
assembleStampedExpr = assembleExpr_helper
  { getId: \_path (Label l) -> l.id
  , getAnnotations: const none
  }

assembleExpr :: forall r. AssembleExpr (Label C r)
assembleExpr = assembleExpr_helper
  { getId: \path _ -> fromPathToString path
  , getAnnotations: const none
  }

increaseIndentLevel = local \ctx -> ctx { indentLevel = ctx.indentLevel + 1 }

assembleExpr_helper
  :: forall r
   . { getId :: Path -> Label C r -> String
     , getAnnotations :: Label C r -> Maybe Ann
     }
  -> AssembleExpr (Label C r)
assembleExpr_helper opts args = Tuple (pure args.label) do
  let id = opts.getId args.path args.label
  ctx <- ask
  elems <- case (args.label # getCon) /\ args.points /\ args.kids of

    -- Root
    C "Root" /\ ps /\ ks -> assembleSimple (pure args.label) ps ks # snd

    -- LineBreak
    C "LineBreak" /\ _ /\ [] -> pure $ fold [ tokens_ghost (id <> "_marker") "⏎", tokens_break (id <> "_break"), tokens_indentation ctx.indentLevel (id <> "_indentation") ]

    -- Lam
    C "Lam" /\ _ /\ [ k_params, k_body ] -> do
      k_params' <- increaseIndentLevel do k_params # snd
      k_body' <- increaseIndentLevel do k_body # snd
      pure $ fold [ tokens_punctuation (id <> "_lambda") "(fun", k_params', tokens_punctuation (id <> "_arrow") "⇒", k_body', tokens_punctuation (id <> "_end") ")" ]
    C "Lam_params" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: none } args id ps ks
    C "Lam_body" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks

    C "App" /\ ps /\ ks -> pure (tokens_punctuation (id <> "_begin") "(") <> assembleAdvanced { targetKidsLength: none } args id ps ks <> pure (tokens_punctuation (id <> "_end") ")")

    -- Let
    C "Let" /\ _ /\ [ k_param, k_impl, k_body ] -> do
      k_param' <- increaseIndentLevel do k_param #. snd
      k_impl' <- increaseIndentLevel do k_impl #. snd
      k_body' <- k_body #. snd
      pure $ fold [ tokens_punctuation (id <> "_let") "(let", k_param', tokens_punctuation (id <> "_assign") "=", k_impl', tokens_punctuation (id <> "_in") "in", k_body', tokens_punctuation (id <> "_end") ")" ]
    C "Let_param" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks
    C "Let_impl" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks
    C "Let_body" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks

    -- Hole
    C "Hole" /\ _ /\ [] -> pure $ tokens_literal id "?"

    C "Int" /\ _ /\ [ Just (Label { con: C x }) /\ _ ] -> pure $ tokens_value id x
    C "Boolean" /\ _ /\ [ Just (Label { con: C x }) /\ _ ] -> pure $ tokens_value id x

    -- Var
    C "Var" /\ _ /\ [ k_lit ] -> k_lit #. snd

    -- Literal
    C lit /\ _ /\ [] -> pure $ tokens_literal id lit

    -- foreign
    C _ /\ _ /\ _ -> assembleExpr_default id args # snd

  let mb_ann = opts.getAnnotations args.label
  pure $ fold $ fold
    [ mb_ann # foldMap \(Ann ann) ->
        [ if null ann.annotations then []
          else
            [ (id <> "_ann_point") /\
                HH.div [ HP.id (id <> "_ann_point"), classes [ "AnnotationPoint" ] ]
                  [ HH.div [ classes [ "label" ] ] $ ann.annotations # map case _ of
                      Info_Annotation _ -> HH.span [ classes [ "Info" ] ] [ HH.text "💡" ]
                      Error_Annotation _ -> HH.span [ classes [ "Error" ] ] [ HH.text "❌" ]
                  ]
            ]
        , [ (id <> "_ann") /\ do
              HH.div [ HP.id (id <> "_ann"), classes [ "Annotations" ] ]
                [ HH.div [ classes [ "inner" ] ] $ ann.annotations # map case _ of
                    Info_Annotation e -> HH.div [ classes [ "item", "Info" ] ] [ e # fromPlainHTML ]
                    Error_Annotation e -> HH.div [ classes [ "item", "Error" ] ] [ e # fromPlainHTML ]
                ]
          ]
        , [ (id <> "_ann_point_sep") /\
              HH.div [ HP.id (id <> "_ann_point_sep"), classes [ "AnnotationSep" ] ]
                []
          ]
        ]
    , [ elems ]
    ]

assembleAdvanced
  :: forall r w i
   . { targetKidsLength :: Maybe Int
     }
  -> { path :: Path
     , label :: Label C r
     , kids :: Array (RenderKid (Label C r) w i)
     , points :: Array (KeyHTML w i)
     }
  -> String
  -> Array (KeyHTML w i)
  -> Array (RenderKid (Label C r) w i)
  -> RenderM (Array (KeyHTML w i))
assembleAdvanced opts args id ps ks = fold
  [ if ks # Array.filter isntFormatting_RenderKid # null then pure (tokens_missing id) else mempty
  , if excessiveKids then fold
      [ pure (tokens_error (id <> "_excessiveKids_begin") "[")
      , assembleSimple (pure args.label) ps (ks # mapWithIndex (\i -> map (_ <> pure (tokens_error (id <> "_excessiveKids_sep_" <> show i) " |")))) #. snd
      , pure (tokens_error (id <> "_excessiveKids_end") "]")
      ]
    else
      assembleSimple (pure args.label) ps ks #. snd
  ]
  where
  excessiveKids = opts.targetKidsLength # maybe false \targetKidsLength ->
    ks #. Array.filter isntFormatting_RenderKid #. length > targetKidsLength

assembleSimple :: forall l w i. Maybe l -> Array (KeyHTML w i) -> Array (RenderKid l w i) -> RenderKid l w i
assembleSimple l ps ks = l /\ do
  ks' <- ks # traverse snd
  pure $ fold $ fold $
    [ Array.zipWith (\p k -> [ p ] <> k) ps ks'
    , [ ps # Array.last # Unfoldable.fromMaybe ]
    ]

--------------------------------------------------------------------------------
-- exprs and zippers
--------------------------------------------------------------------------------

expr_LineBreak = C "LineBreak" % []

expr_Var x = C "Var" % [ C x % [] ]

expr_Lam = C "Lam" % [ C "Lam_params" % [], C "Lam_body" % [] ]
zipper_Lam_params = (expr_Lam # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_Lam_body = (expr_Lam # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

expr_App = C "App" % []
zipper_App = (expr_App # atPoint (Point { path: mempty, j: Index 0 })).outside # fromSpanContextToZipper

expr_Let = C "Let" % [ C "Let_param" % [], C "Let_impl" % [], C "Let_body" % [] ]
zipper_Let_param = (expr_Let # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_Let_impl = (expr_Let # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_Let_body = (expr_Let # atPoint (Point { path: Step 2 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

expr_Hole = C "Hole" % []

expr_Int x = C "Int" % [ C (show @Int x) % [] ]
expr_Boolean x = C "Boolean" % [ C (show @Boolean x) % [] ]

--------------------------------------------------------------------------------
-- isValidHandle
--------------------------------------------------------------------------------

isValidHandle :: forall r. (Expr (Label C r)) -> Handle -> Boolean
isValidHandle root handle =
  case handle of
    Point_Handle p -> and [ isValidPoint root p ]
    SpanH_Handle sh _ -> and [ isValidPoint root p._L, isValidPoint root p._R ]
      where
      p = getEndPoints_SpanH sh
    ZipperH_Handle zh _ -> and [ isValidPoint root p._OL, isValidPoint root p._IL, isValidPoint root p._IR, isValidPoint root p._OR ]
      where
      p = getEndPoints_ZipperH zh

isValidPoint :: forall r. Expr (Label C r) -> Point -> Boolean
isValidPoint e0 (Point p) = (e.l # getCon) `Set.member` constructors_canHaveAnyNumberOrKids
  where
  Expr e = (e0 # atSubExpr p.path).here

--------------------------------------------------------------------------------
-- isHole
--------------------------------------------------------------------------------

isHole e0 (Point p) = and
  [ constructors_expectsNonzeroKids #. Set.member l.con
  , kids #. Array.filter (\(Expr { l: Label l }) -> l.con /= C "LineBreak") #. length == 0
  ]
  where
  Expr { l: Label l, kids } = (e0 # atSubExpr p.path).here

--------------------------------------------------------------------------------
-- constructor classes
--------------------------------------------------------------------------------

constructors_expectsNonzeroKids = Set.fromFoldable
  [ C "Let_param"
  , C "Let_impl"
  , C "Let_body"
  , C "Lam_params"
  , C "Lam_body"
  , C "App_func"
  , C "App_args"
  ]

constructors_canHaveAnyNumberOrKids = Set.fromFoldable $ fold
  [ [ C "Root" ]
  , [ C "Lam_params", C "Lam_body" ]
  , [ C "App" ]
  , [ C "Let_param", C "Let_impl", C "Let_body" ]
  ]

--------------------------------------------------------------------------------
-- tokens
--------------------------------------------------------------------------------

tokens_punctuation key str = [ mk_token key [ "punctuation" ] (pure str) ]

tokens_ghost key str = [ mk_token key [ "ghost" ] (pure str) ]

tokens_break key = [ mk_token key [ "break", "ghost" ] none ]

tokens_indentation n key =
  mk_token key [ "indentation", "ghost" ] (pure "│")
    # Array.replicate n
    # mapWithIndex \i (key' /\ e) -> (key' <> "_" <> show i) /\ e

tokens_literal key str = [ mk_token key [ "literal" ] (pure str) ]

tokens_value key str = [ mk_token key [ "value" ] (pure str) ]

tokens_missing key = [ mk_token (key <> "_missing") [ "missing" ] none ]

tokens_error key str = [ mk_token (key <> "_error") [ "error" ] (pure str) ]

mk_token key cs Nothing = key /\ HH.div [ HP.id key, classes ([ "Token" ] <> cs) ] []
mk_token key cs (Just str) = key /\ HH.div [ HP.id key, classes ([ "Token" ] <> cs) ] [ HH.text str ]

--------------------------------------------------------------------------------
-- html
--------------------------------------------------------------------------------

linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation