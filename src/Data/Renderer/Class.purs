module Data.Renderer.Class
  ( class Renderer
  , render
  ) where

import Prelude

import Data.Game (Game)

class Monad m <= Renderer m where
  render :: Game -> m Unit
