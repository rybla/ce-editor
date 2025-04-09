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
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref as Ref
import Ui.Component (Component)
import Ui.Component as Component
import Utility (todo)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget

main :: Aff Unit
main = do
  Component.root
    [ editorComponent ]
    # liftEffect
  pure unit

editorComponent :: Effect Component
editorComponent = do
  rootRef <- Ref.new $ Nothing @Component
  let
    getRoot = rootRef # Ref.read >>= case _ of
      Nothing -> throw "getRoot: root hasn't been created yet"
      Just root -> pure root

  handleRef <- Ref.new $ Point { path: Nil, j: Index 0 }

  let
    fromStepToExprComponentKidIndex :: Step -> Int
    fromStepToExprComponentKidIndex (Step i) = 1 + 1 + (2 * i)

    fromIndexToExprComponentKidIndex :: Index -> Int
    fromIndexToExprComponentKidIndex (Index i) = 1 + (2 * i)

    getPointComponent :: Point -> Effect Component
    getPointComponent p0 = go p0 =<< getRoot
      where
      go :: Point -> Component -> Effect Component
      go (Point p) c = case p.path of
        Nil -> c # Component.getKid (p.j # fromIndexToExprComponentKidIndex)
        Cons i path' -> do
          kid <- c # Component.getKid (i # fromStepToExprComponentKidIndex)
          kid # go (Point { path: path', j: p.j })

    getExprComponent :: Path -> Effect Component
    getExprComponent path0 = go path0 =<< getRoot
      where
      go :: Path -> Component -> Effect Component
      go path c = case path of
        Nil -> pure c
        Cons i path' -> do
          kid <- c # Component.getKid (i # fromStepToExprComponentKidIndex)
          kid # go path'

    onClick_PointComponent :: Point -> Effect Unit
    onClick_PointComponent (Point p) = do
      -- unset old handle
      do
        handle <- handleRef # Ref.read
        c <- getPointComponent handle
        c # Component.removeClass "Focus"
      -- update handle
      handleRef # Ref.write (Point p)
      -- set new handle
      do
        handle <- handleRef # Ref.read
        c <- getPointComponent handle
        c # Component.addClass "Focus"

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
                  onClick_PointComponent (Point p)
              , passive: true
              , capture: false
              , once: false
              }
            ]
        }
        [ Component.newText {} " " ]

  root <- exprComponent Nil (Expr { l: Root, kids: [ example_expr 2 2 ] })
  rootRef # Ref.write (Just root)
  app <- Component.newTree {} [ pure root ]

  -- do
  --   handle <- handleRef # Ref.read
  --   c <- getPointComponent handle
  --   c # Component.addClass "Focus"

  pure app

