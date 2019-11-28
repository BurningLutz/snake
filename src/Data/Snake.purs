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

import Data.Array (foldM)
import Data.Array.NonEmpty (NonEmptyArray, init, singleton, snoc, snoc', uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vector (Vector, uvecA, uvecD, uvecS, uvecW)

data Direction
  = W
  | S
  | A
  | D

toVector :: Direction -> Vector
toVector W = uvecW
toVector S = uvecS
toVector A = uvecA
toVector D = uvecD

instance showDirection :: Show Direction where
  show W = "↑"
  show S = "↓"
  show A = "←"
  show D = "→"

type Point = Int /\ Int

type Head = Point
type Body = NonEmptyArray Direction

type Snake = 
  { direction :: Direction
  , head :: Head
  , body :: Body
  }

prevBody :: Direction -> Point -> Point
prevBody dir p = p - toVector dir

nextBody :: Direction -> Point -> Point
nextBody dir p = p + toVector dir

makeSnakeTowards :: Direction -> Head -> Snake
makeSnakeTowards direction head = do
  let
    body = singleton direction

  { direction
  , head
  , body
  }

move :: Snake -> Snake
move { direction, head, body } = do
  let
    x /\ y = head
    newBody = snoc' (init body) direction
    newHead = nextBody direction head

  { direction
  , head : newHead
  , body : newBody
  }

grow :: Snake -> Snake
grow { direction, head, body } = do
  let
    x /\ y = head
    newBody = snoc body direction
    newHead = nextBody direction head

  { direction
  , head : newHead
  , body : newBody
  }

canBiteItself :: Snake -> Boolean
canBiteItself { body, head } = do
  let
    { head : head', tail } = uncons body
    m = foldM <@> toVector head' <@> tail $ \acc p -> do
          if acc == 0 /\ 0
            then Nothing
            else Just (acc + toVector p)

  -- we find the last point that will not be bitten, Nothing means there
  -- is at least one point been bitten
  case m of
    Nothing -> true
    Just _  -> false
