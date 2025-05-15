module Editor.Example.SlcV1 where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Reader (ask, local, runReader, runReaderT)
import Data.Array as Array
import Data.Expr (Expr(..), Fragment(..), Handle(..), Index(..), Path, Point(..), Span(..), Step(..), BasicEditorState, atPoint, atSubExpr, fromPathToString, fromPointToString, fromSpanContextToZipper, getEndPoints_SpanH, getEndPoints_ZipperH, mkExpr, mkSpanTooth, mkTooth, stampTraversable)
import Data.Expr.Edit as Expr.Edit
import Data.Expr.Render (Annotation(..), AssembleExpr, KeyHTML, RenderArgs, RenderKid, RenderM)
import Data.Expr.Render as Expr.Render
import Data.Foldable (and, fold, foldMap, length, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe, none)
import Editor.Common (Diagnostic(..), Editor(..), Label(..), StampedLabel, AnnotatedLabel, assembleExpr_default, getCon)
import Effect.Aff (Aff)
import Halogen.HTML (fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Properties (id)
import Halogen.HTML.Properties as HP
import Record as Record
import Type.Proxy (Proxy(..))
import Ui.Event (keyEq, matchKeyInfoPattern', not_alt, not_cmd)
import Ui.Halogen (classes)
import Utility (collapse, isIdentifierOrNumeric, (#.))

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

editor :: Editor C
editor = Editor
  { name: "scoped lambda calculus"
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
  zipper_App_func' <- zipper_App_func # stampTraversable
  edit_App_func <- Tuple "App_func" <$> Expr.Edit.insert (Zipper_Fragment zipper_App_func') state
  zipper_App_args' <- zipper_App_args # stampTraversable
  edit_App_args <- Tuple "App_args" <$> Expr.Edit.insert (Zipper_Fragment zipper_App_args') state
  -- Let
  zipper_Let_param' <- zipper_Let_param # stampTraversable
  edit_Let_param <- Tuple "Let_param" <$> Expr.Edit.insert (Zipper_Fragment zipper_Let_param') state
  zipper_Let_impl' <- zipper_Let_impl # stampTraversable
  edit_Let_impl <- Tuple "Let_impl" <$> Expr.Edit.insert (Zipper_Fragment zipper_Let_impl') state
  zipper_Let_body' <- zipper_Let_body # stampTraversable
  edit_Let_body <- Tuple "Let_body" <$> Expr.Edit.insert (Zipper_Fragment zipper_Let_body') state
  pure \query -> do
    case query of
      "fun" -> pure [ edit_Lam_params, edit_Lam_body ]
      "app" -> pure [ edit_App_func, edit_App_args ]
      "let" -> pure [ edit_Let_param, edit_Let_impl, edit_Let_body ]
      -- Var
      _ | query # isIdentifierOrNumeric -> do
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
      zipper_App_func' <- zipper_App_func # stampTraversable
      Expr.Edit.insert (Zipper_Fragment zipper_App_func') state
  | ki # matchKeyInfoPattern' [ keyEq ")", not_cmd, not_alt ] = do
      zipper_App_args' <- zipper_App_args # stampTraversable
      Expr.Edit.insert (Zipper_Fragment zipper_App_args') state
  | otherwise = empty

--------------------------------------------------------------------------------
-- printExpr
--------------------------------------------------------------------------------

printExpr = go
  where
  go = case _ of
    Expr { l: Label { con: C "Root" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "Var" }, kids: [ Expr { l: Label { con: C x } } ] } -> x
    Expr { l: Label { con: C "Let" }, kids: [ param@(Expr { l: Label { con: C "Let_param" } }), impl@(Expr { l: Label { con: C "Let_impl" } }), body@(Expr { l: Label { con: C "Let_body" } }) ] } -> "(let " <> (param # go) <> " = " <> (impl # go) <> " in " <> (body # go) <> ")"
    Expr { l: Label { con: C "Let_param" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "Let_impl" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "Let_body" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "Lam" }, kids: [ params@(Expr { l: Label { con: C "Lam_params" } }), body@(Expr { l: Label { con: C "Lam_body" } }) ] } -> "(fun " <> (params # go) <> " ⇒ " <> (body # go) <> ")"
    Expr { l: Label { con: C "Lam_params" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "Lam_body" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "App" }, kids: [ func@(Expr { l: Label { con: C "App_func" } }), args@(Expr { l: Label { con: C "App_args" } }) ] } -> "(" <> (func # go) <> " " <> (args # go) <> ")"
    Expr { l: Label { con: C "App_func" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "App_args" }, kids } -> kids # map go # String.joinWith " "
    Expr { l: Label { con: C "LineBreak" }, kids: [] } -> "\n"
    e -> show e

--------------------------------------------------------------------------------
-- getDiagnostics
--------------------------------------------------------------------------------

getDiagnostics :: forall rA rB. BasicEditorState (Label C rA) (AnnotatedLabel C rB) -> Array Diagnostic
getDiagnostics state = collapse @Array @Maybe
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

annotateExpr :: forall r. Expr (StampedLabel C r) -> Aff (Expr (AnnotatedLabel C r))
annotateExpr e0 = runReaderT (go e0) ctx0
  where
  ctx0 =
    { scope: Set.empty :: Set String
    }

  -- Lam
  go (Expr { l: Label l@{ con: C "Lam" }, kids: [ e_params@(Expr { l: Label { con: C "Lam_params" }, kids: es_params }), k_body ] })
    | Just xs <-
        es_params
          # map
              ( case _ of
                  Expr { l: Label { con: C "Var" }, kids: [ Expr { l: Label { con: C x } } ] } -> pure x
                  _ -> none
              )
          # sequence
          # map Set.fromFoldable = do
        e_params' <- local (Record.modify (Proxy @"scope") (Set.union xs)) do
          e_params # go
        k_body' <- local (Record.modify (Proxy @"scope") (Set.union xs)) do
          k_body # go
        pure $ Expr { l: Label $ l # Record.union { ann: none }, kids: [ e_params', k_body' ] }

  -- App
  go (Expr { l: Label l@{ con: C "App" }, kids: [ k_func, k_args ] }) = do
    k_func' <- k_func # go
    k_args' <- k_args # go
    pure $ Expr { l: Label $ l # Record.union { ann: none }, kids: [ k_func', k_args' ] }

  -- Let
  go (Expr { l: Label l@{ con: C "Let" }, kids: [ k_param@(Expr { l: Label { con: C "Let_param" }, kids: params }), k_impl, k_body ] }) = do
    let
      xs = params # foldMap case _ of
        Expr { l: Label { con: C "Var" }, kids: [ Expr { l: Label { con: C x } } ] } -> Set.singleton x
        _ -> Set.empty
    k_param' <- local (Record.modify (Proxy @"scope") (Set.union xs)) do
      k_param # go
    k_impl' <- local (Record.modify (Proxy @"scope") (Set.union xs)) do
      k_impl # go
    k_body' <- local (Record.modify (Proxy @"scope") (Set.union xs)) do
      k_body # go
    pure $ Expr { l: Label $ l # Record.union { ann: none }, kids: [ k_param', k_impl', k_body' ] }

  -- Var
  go (Expr { l: Label l@{ con: C "Var" }, kids: [ k_label@(Expr { l: Label { con: C x } }) ] }) = do
    ctx <- ask
    let
      ann = fold
        [ if ctx.scope # Set.member x then [] else [ Error_Annotation $ HH.text "variable not in scope" ]
        ]
    k_label' <- k_label # go
    pure $ Expr { l: Label $ l # Record.union { ann: if null ann then none else pure ann }, kids: [ k_label' ] }
  -- 
  go e = go_skip e

  go_skip (Expr { l: Label l, kids }) = do
    kids' <- kids # traverse go
    pure $ Expr { l: Label $ l # Record.union { ann: none }, kids: kids' }

-- annotateExpr :: forall r. Expr (StampedLabel C r) -> Aff (Expr (AnnotatedLabel C r))
-- annotateExpr = traverse \(Label l) -> pure $ Label $ Record.union { ann: none } l

--------------------------------------------------------------------------------
-- RenderKid predicates
--------------------------------------------------------------------------------

isntFormatting_RenderKid :: forall r w i. RenderKid (Label C r) w i -> Boolean
isntFormatting_RenderKid = fst >>> maybe true \(Label l) -> l.con /= C "LineBreak"

--------------------------------------------------------------------------------
-- assembly
--------------------------------------------------------------------------------

assembleAnnotatedExpr :: forall r. AssembleExpr (AnnotatedLabel C r)
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
     , getAnnotations :: Label C r -> Maybe (Array Annotation)
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

    -- App
    C "App" /\ _ /\ [ k_func, k_args ] -> do
      k_func' <- increaseIndentLevel do k_func #. snd
      k_args' <- increaseIndentLevel do k_args #. snd
      pure $ fold [ tokens_punctuation (id <> "_begin") "(", k_func', tokens_punctuation (id <> "_op") "$", k_args', tokens_punctuation (id <> "_end") ")" ]
    C "App_func" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks
    C "App_args" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks

    -- Let
    C "Let" /\ _ /\ [ k_param, k_impl, k_body ] -> do
      k_param' <- increaseIndentLevel do k_param #. snd
      k_impl' <- increaseIndentLevel do k_impl #. snd
      k_body' <- k_body #. snd
      pure $ fold [ tokens_punctuation (id <> "_let") "(let", k_param', tokens_punctuation (id <> "_assign") "=", k_impl', tokens_punctuation (id <> "_in") "in", k_body', tokens_punctuation (id <> "_end") ")" ]
    C "Let_param" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks
    C "Let_impl" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks
    C "Let_body" /\ ps /\ ks -> assembleAdvanced { targetKidsLength: pure 1 } args id ps ks

    -- Var
    C "Var" /\ _ /\ [ k_lit ] -> k_lit #. snd

    -- Literal
    C lit /\ _ /\ [] -> pure $ tokens_literal id lit

    -- foreign
    C _ /\ _ /\ _ -> assembleExpr_default id args # snd

  let mb_ann = opts.getAnnotations args.label
  pure $ fold $ fold
    [ mb_ann # foldMap \anns ->
        [ [ (id <> "_ann_point") /\
              HH.div [ HP.id (id <> "_ann_point"), classes [ "AnnotationPoint" ] ]
                [ HH.div [ classes [ "label" ] ] $ anns # map case _ of
                    Info_Annotation _ -> HH.span [ classes [ "Info" ] ] [ HH.text "💡" ]
                    Error_Annotation _ -> HH.span [ classes [ "Error" ] ] [ HH.text "❌" ]
                ]
          ]
        , [ (id <> "_ann") /\ do
              HH.div [ HP.id (id <> "_ann"), classes [ "Annotations" ] ]
                [ HH.div [ classes [ "inner" ] ] $ anns # map case _ of
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
    , [ ps # Array.last # fromMaybe ]
    ]

--------------------------------------------------------------------------------
-- exprs and zippers
--------------------------------------------------------------------------------

expr_LineBreak = C "LineBreak" % []

expr_Var x = C "Var" % [ C x % [] ]

expr_Lam = C "Lam" % [ C "Lam_params" % [], C "Lam_body" % [] ]
zipper_Lam_params = (expr_Lam # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_Lam_body = (expr_Lam # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

expr_App = C "App" % [ C "App_func" % [], C "App_args" % [] ]
zipper_App_func = (expr_App # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_App_args = (expr_App # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

expr_Let = C "Let" % [ C "Let_param" % [], C "Let_impl" % [], C "Let_body" % [] ]
zipper_Let_param = (expr_Let # atPoint (Point { path: Step 0 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_Let_impl = (expr_Let # atPoint (Point { path: Step 1 : Nil, j: Index 0 })).outside # fromSpanContextToZipper
zipper_Let_body = (expr_Let # atPoint (Point { path: Step 2 : Nil, j: Index 0 })).outside # fromSpanContextToZipper

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
isValidPoint e0 (Point p) = (e.l # getCon) `Set.member` constructors_valid
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

constructors_needParensWhenOnLeftOfApp = Set.fromFoldable
  [ C "Lam"
  , C "Let"
  ]

constructors_needParensWhenOnRightOfApp = Set.fromFoldable
  [ C "Lam"
  , C "Let"
  ]

constructors_valid = Set.fromFoldable $ fold
  [ [ C "Root" ]
  , [ C "Lam_params", C "Lam_body" ]
  , [ C "App_func", C "App_args" ]
  , [ C "Let_param", C "Let_impl", C "Let_body" ]
  ]

--------------------------------------------------------------------------------
-- html
--------------------------------------------------------------------------------

linebreak = [ HH.div [ classes [ "Token punctuation ghost" ] ] [ HH.text "⏎" ], HH.div [ classes [ "Token break" ] ] [] ]
indentation = [ HH.div [ classes [ "Token punctuation indentation ghost" ] ] [ HH.text "│" ] ]
indentations n = fold $ Array.replicate n indentation

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

tokens_missing key = [ mk_token (key <> "_missing") [ "missing" ] none ]

tokens_error key str = [ mk_token (key <> "_error") [ "error" ] (pure str) ]

mk_token key cs Nothing = key /\ HH.div [ HP.id key, classes ([ "Token" ] <> cs) ] []
mk_token key cs (Just str) = key /\ HH.div [ HP.id key, classes ([ "Token" ] <> cs) ] [ HH.text str ]
