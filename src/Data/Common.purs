module Data.Common
  ( (~)
  , between'
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))

infix 0 Tuple as ~

between' :: forall a. Ord a => a -> a /\ a -> Boolean
between' i (l /\ u) = between l u i
