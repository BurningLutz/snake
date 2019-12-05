module Data.Common
  ( (~)
  , between'

  , Vector
  , Point
  , uvecW
  , uvecS
  , uvecA
  , uvecD
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))

infix 0 Tuple as ~

between' :: forall a. Ord a => a -> a /\ a -> Boolean
between' i (l /\ u) = between l u i

type Vector = Int /\ Int
type Point = Int /\ Int

uvecW :: Vector
uvecW = 0 /\ -1

uvecS :: Vector
uvecS = 0 /\ 1

uvecA :: Vector
uvecA = -1 /\ 0

uvecD :: Vector
uvecD = 1 /\ 0
