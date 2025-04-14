module Ui.AppV0 where

-- import Prelude

-- import Data.Array as Array
-- import Data.Expr (Expr(..), Index(..), Path, Point(..), Step(..))
-- import Data.FoldableWithIndex (foldMapWithIndex)
-- import Data.List (List(..))
-- import Data.List as List
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..))
-- import Data.Tuple.Nested ((/\))
-- import Editor.Example.Editor1 (L(..), L'(..), example_expr)
-- import Effect (Effect)
-- import Effect.Class.Console as Console
-- import Effect.Exception (throw)
-- import Effect.Ref (Ref)
-- import Effect.Ref as Ref
-- import Ui.Component (Component)
-- import Ui.Component as Component
-- import Web.DOM (Element)
-- import Web.Event.Event (EventType(..))
-- import Web.Event.Event as Event
-- import Web.Event.EventTarget as EventTarget

-- --------------------------------------------------------------------------------
-- -- Config
-- --------------------------------------------------------------------------------

-- config =
--   { initialExpr: example_expr {} 2 2
--   }

-- --------------------------------------------------------------------------------
-- -- types
-- --------------------------------------------------------------------------------

-- type PreLabel = L {}

-- type UiLabel = L { elem :: Element }

-- --------------------------------------------------------------------------------
-- -- main
-- --------------------------------------------------------------------------------

-- type State =
--   { rootExprComponent :: Ref (Maybe Component)
--   , handle :: Ref (Maybe Point)
--   }

-- main :: Effect Unit
-- main = do
--   state <- newState

--   rootExprComponent <-
--     newExprComponent state
--       Nil
--       (Expr { l: L Root {}, kids: [ config.initialExpr ] })
--   state.rootExprComponent # Ref.write (Just rootExprComponent)

--   editorComponent <-
--     newEditorComponent state rootExprComponent

--   Component.root
--     [ pure editorComponent ]

--   initialize state

-- --------------------------------------------------------------------------------
-- -- EditorComponent
-- --------------------------------------------------------------------------------

-- newEditorComponent :: State -> Component -> Effect Component
-- newEditorComponent _state rootExprComponent = do
--   Component.newTree
--     { name: pure "Editor"
--     , attributes: Map.fromFoldable
--         [ "class" /\ "Editor" ]
--     }
--     [ pure rootExprComponent ]

-- --------------------------------------------------------------------------------
-- -- ExprComponent
-- --------------------------------------------------------------------------------

-- newExprComponent :: State -> Path -> Expr PreLabel -> Effect Component
-- newExprComponent state path (Expr e) = do
--   Component.newTree
--     { name: pure "Expr"
--     , attributes: Map.fromFoldable
--         [ "class" /\ "Expr" ]
--     , eventListeners:
--         [ { eventType: EventType "click"
--           , eventListener: EventTarget.eventListener \event -> do
--               event # Event.stopPropagation
--               Console.log $ "clicked on Expr: " <> show path
--           , passive: true
--           , capture: false
--           , once: false
--           }
--         ]
--     }
--     ( Array.fold
--         [ [ Component.newText
--               { name: pure "Label"
--               , attributes: Map.fromFoldable
--                   [ "class" /\ "Label" ]
--               }
--               (show e.l)
--           ]
--         , if Array.null e.kids then
--             [ newPointComponent state (Point { path, j: Index 0 }) ]
--           else Array.fold
--             [ e.kids # foldMapWithIndex \i e' ->
--                 [ newPointComponent state (Point { path, j: Index i })
--                 , newExprComponent state (path `List.snoc` Step i) e'
--                 ]
--             , [ newPointComponent state (Point { path, j: Index (e.kids # Array.length) }) ]
--             ]
--         ]
--     )

-- --------------------------------------------------------------------------------
-- -- PointComponent
-- --------------------------------------------------------------------------------

-- newPointComponent :: State -> Point -> Effect Component
-- newPointComponent state (Point p) = do
--   Component.newTree
--     { name: pure "Point"
--     , attributes: Map.fromFoldable
--         [ "class" /\ "Point" ]
--     , eventListeners:
--         [ { eventType: EventType "click"
--           , eventListener: EventTarget.eventListener \event -> do
--               event # Event.stopPropagation
--               Console.log $ "clicked on Point: " <> show (Point p)
--               setHandle state (Point p)
--           , passive: true
--           , capture: false
--           , once: false
--           }
--         ]
--     }
--     [ Component.newText {} " " ]

-- --------------------------------------------------------------------------------
-- -- State
-- --------------------------------------------------------------------------------

-- newState :: Effect State
-- newState = do
--   rootExprComponent <- Ref.new $ Nothing @Component
--   handle <- Ref.new $ Nothing
--   pure
--     { rootExprComponent
--     , handle
--     }

-- initialize :: State -> Effect Unit
-- initialize state = do
--   setHandle state $ Point { path: Nil, j: Index 0 }

-- getRootExprComponent :: State -> Effect Component
-- getRootExprComponent state = state.rootExprComponent # Ref.read >>= case _ of
--   Nothing -> throw "getRootExprComponent: root hasn't been created yet"
--   Just root -> pure root

-- getExprComponent :: State -> Path -> Effect Component
-- getExprComponent state path0 = go path0 =<< getRootExprComponent state
--   where
--   go :: Path -> Component -> Effect Component
--   go path c = case path of
--     Nil -> pure c
--     Cons i path' -> do
--       kid <- c # Component.getKid (i # fromStepToExprComponentKidIndex)
--       kid # go path'

-- getPointComponent :: State -> Point -> Effect Component
-- getPointComponent state p0 = go p0 =<< getRootExprComponent state
--   where
--   go :: Point -> Component -> Effect Component
--   go (Point p) c = case p.path of
--     Nil -> c # Component.getKid (p.j # fromIndexToExprComponentKidIndex)
--     Cons i path' -> do
--       kid <- c # Component.getKid (i # fromStepToExprComponentKidIndex)
--       kid # go (Point { path: path', j: p.j })

-- setHandle :: State -> Point -> Effect Unit
-- setHandle state (Point p) = do
--   -- deactivate old handle
--   state.handle # Ref.read >>= case _ of
--     Nothing -> pure unit
--     Just handle -> do
--       c <- getPointComponent state handle
--       c # Component.removeClass "Focus"
--   -- update handle
--   state.handle # Ref.write (Just (Point p))
--   -- activate new handle
--   do
--     c <- getPointComponent state (Point p)
--     c # Component.addClass "Focus"

-- --------------------------------------------------------------------------------
-- -- Operations
-- --------------------------------------------------------------------------------

-- fromStepToExprComponentKidIndex :: Step -> Int
-- fromStepToExprComponentKidIndex (Step i) = 1 + 1 + (2 * i)

-- fromIndexToExprComponentKidIndex :: Index -> Int
-- fromIndexToExprComponentKidIndex (Index i) = 1 + (2 * i)

-- --------------------------------------------------------------------------------
-- -- Utilities
-- --------------------------------------------------------------------------------

