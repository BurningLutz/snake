module Data.Vector
  ( Vector
  , uvecW
  , uvecS
  , uvecA
  , uvecD
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

type Vector = Int /\ Int

uvecW :: Vector
uvecW = 0 /\ -1

uvecS :: Vector
uvecS = 0 /\ 1

uvecA :: Vector
uvecA = -1 /\ 0

uvecD :: Vector
uvecD = 1 /\ 0
