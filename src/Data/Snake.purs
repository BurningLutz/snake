module Data.Snake
  ( Direction(..)
  , Point
  , Head
  , Body
  , Snake

  , makeSnakeTowards
  , move
  , grow
  , canBiteItself
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, findIndex, init, singleton, snoc, snoc')
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

data Direction
  = W
  | S
  | A
  | D

instance showDirection :: Show Direction where
  show W = "↑"
  show S = "↓"
  show A = "←"
  show D = "→"

type Point = Int /\ Int

type Head = Point
type Body = NonEmptyArray Point

type Snake = 
  { direction :: Direction
  , head :: Head
  , body :: Body
  }

prevBody :: Direction -> Point -> Point
prevBody direction (x /\ y) = case direction of
  W -> (x /\ (y + 1))
  S -> (x /\ (y - 1))
  A -> ((x + 1) /\ y)
  D -> ((x - 1) /\ y)

nextBody :: Direction -> Point -> Point
nextBody direction (x /\ y) = case direction of
  W -> (x /\ (y - 1))
  S -> (x /\ (y + 1))
  A -> ((x - 1) /\ y)
  D -> ((x + 1) /\ y)

makeSnakeTowards :: Direction -> Head -> Snake
makeSnakeTowards direction head@(x /\ y) = do
  let
    body = singleton (prevBody direction head)

  { direction
  , head
  , body
  }

move :: Snake -> Snake
move { direction, head, body } = do
  let
    x /\ y = head
    newBody = snoc' (init body) head
    newHead = nextBody direction head

  { direction
  , head : newHead
  , body : newBody
  }

grow :: Snake -> Snake
grow { direction, head, body } = do
  let
    x /\ y = head
    newBody = snoc body head
    newHead = nextBody direction head

  { direction
  , head : newHead
  , body : newBody
  }

canBiteItself :: Snake -> Boolean
canBiteItself { body, head } = do
  case findIndex (_ == head) body of
    Nothing -> false
    Just _  -> true
