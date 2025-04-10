module Ui.App where

import Prelude

import Data.Array as Array
import Data.Expr (Expr(..), Index(..), Label(..), Path, Point(..), Step(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Editor.Example.Editor1 (example_expr)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref as Ref
import Ui.Component (Component)
import Ui.Component as Component
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget

main :: Effect Unit
main = do
  -- rootExprComponentRef
  rootExprComponentRef <- Ref.new $ Nothing @Component
  let
    getRootExprComponent = rootExprComponentRef # Ref.read >>= case _ of
      Nothing -> throw "getRootExprComponent: root hasn't been created yet"
      Just root -> pure root

  -- handleRef
  handleRef <- Ref.new $ Nothing

  let
    getExprComponent :: Path -> Effect Component
    getExprComponent path0 = go path0 =<< getRootExprComponent
      where
      go :: Path -> Component -> Effect Component
      go path c = case path of
        Nil -> pure c
        Cons i path' -> do
          kid <- c # Component.getKid (i # fromStepToExprComponentKidIndex)
          kid # go path'

    getPointComponent :: Point -> Effect Component
    getPointComponent p0 = go p0 =<< getRootExprComponent
      where
      go :: Point -> Component -> Effect Component
      go (Point p) c = case p.path of
        Nil -> c # Component.getKid (p.j # fromIndexToExprComponentKidIndex)
        Cons i path' -> do
          kid <- c # Component.getKid (i # fromStepToExprComponentKidIndex)
          kid # go (Point { path: path', j: p.j })

    setHandle :: Point -> Effect Unit
    setHandle (Point p) = do
      -- deactivate old handle
      handleRef # Ref.read >>= case _ of
        Nothing -> pure unit
        Just handle -> do
          c <- getPointComponent handle
          c # Component.removeClass "Focus"
      -- update handle
      handleRef # Ref.write (Just (Point p))
      -- activate new handle
      do
        c <- getPointComponent (Point p)
        c # Component.addClass "Focus"

  let
    exprComponent :: Path -> Expr -> Effect Component
    exprComponent path (Expr e) = do
      Component.newTree
        { name: pure "Expr"
        , classes: [ "Expr" ]
        , eventListeners:
            [ { eventType: EventType "click"
              , eventListener: EventTarget.eventListener \event -> do
                  event # Event.stopPropagation
                  Console.log $ "clicked on Expr: " <> show path
              , passive: true
              , capture: false
              , once: false
              }
            ]
        }
        ( Array.fold
            [ [ Component.newText
                  { classes: [ "Label" ] }
                  (show e.l)
              ]
            , if Array.null e.kids then
                [ pointComponent (Point { path, j: Index 0 }) ]
              else Array.fold
                [ e.kids # foldMapWithIndex \i e' ->
                    [ pointComponent (Point { path, j: Index i })
                    , exprComponent (path `List.snoc` Step i) e'
                    ]
                , [ pointComponent (Point { path, j: Index (e.kids # Array.length) }) ]
                ]
            ]
        )

    pointComponent :: Point -> Effect Component
    pointComponent (Point p) = do
      Component.newTree
        { name: pure "Point"
        , classes: [ "Point" ]
        , eventListeners:
            [ { eventType: EventType "click"
              , eventListener: EventTarget.eventListener \event -> do
                  event # Event.stopPropagation
                  Console.log $ "clicked on Point: " <> show (Point p)
                  setHandle (Point p)
              , passive: true
              , capture: false
              , once: false
              }
            ]
        }
        [ Component.newText {} " " ]

  -- rootExprComponent

  rootExprComponent <- do
    rootExprComponent <- exprComponent
      Nil
      (Expr { l: Root, kids: [ example_expr 2 2 ] })
    rootExprComponentRef # Ref.write (Just rootExprComponent)
    pure rootExprComponent

  -- editorComponent

  editorComponent <-
    Component.newTree
      {}
      [ pure rootExprComponent ]

  -- Component.root

  Component.root
    [ pure editorComponent ]

  -- initialize

  setHandle $ Point { path: Nil, j: Index 0 }

  pure unit

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

fromStepToExprComponentKidIndex :: Step -> Int
fromStepToExprComponentKidIndex (Step i) = 1 + 1 + (2 * i)

fromIndexToExprComponentKidIndex :: Index -> Int
fromIndexToExprComponentKidIndex (Index i) = 1 + (2 * i)

