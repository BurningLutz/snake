module Data.AppM
  ( appM

  , class GameProvider
  , new
  , read
  , write
  , class Interactive
  , onKey
  , class Suspendable
  , suspend
  , class Quitable
  , quit
  , class Setup
  , setup
  ) where

import Prelude

import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Variant (SProxy(..))
import Data.Game (class MeatSpawner, Game, GameOver, next)
import Data.Game as Game
import Data.GamingArea (HeightOutOfRange, WidthOutOfRange)
import Data.Renderer.Class (class Renderer, render)
import Data.Snake (Direction(..), towards)
import Data.Tuple.Nested ((/\))
import Effect.Ref (Ref)
import Record (modify)
import Type.Row (type (+))

class Monad m <= GameProvider m where
  new :: Game -> m (Ref Game)
  read :: Ref Game -> m Game
  write :: Ref Game -> Game -> m Unit

class Monad m <= Suspendable m where
  suspend :: m Unit

class Monad m <= Interactive m where
  onKey :: String -> m Unit -> m Unit

class Monad m <= Quitable m where
  quit :: m Unit

class Monad m <= Setup m where
  setup :: m Unit

appM
  :: forall r m
   . GameProvider m
  => MeatSpawner m
  => Renderer m
  => Setup m
  => Interactive m
  => Suspendable m
  => Quitable m
  => ExceptV
       ( WidthOutOfRange
       + HeightOutOfRange
       + GameOver
       + r
       )
       m Unit
appM = do
  lift setup

  game    <- Game.new 80 30 D (40 /\ 25)
  gameRef <- lift $ new game

  let
    towards' :: Direction -> m Unit
    towards' dir = do
      read gameRef
        <#> modify (SProxy :: SProxy "snake") (towards dir)
        >>= write gameRef

  -- key bindings
  lift $ onKey "w" (towards' W)
  lift $ onKey "s" (towards' S)
  lift $ onKey "a" (towards' A)
  lift $ onKey "d" (towards' D)

  -- main loop
  let
    tick = do
      currentGame <- lift $ read gameRef
      lift $ render currentGame

      nextGame <- next currentGame
      lift do
        write gameRef nextGame
        suspend

      tick

  tick
