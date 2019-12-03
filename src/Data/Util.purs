module Data.Util
  ( setRawMode
  ) where

import Prelude

import Effect (Effect)
import Node.Stream (Read, Stream)

foreign import setRawModeImpl :: forall r. Stream (read :: Read | r) -> Boolean -> Effect Unit

setRawMode :: forall r. Stream (read :: Read | r) -> Boolean -> Effect Unit
setRawMode = setRawModeImpl
