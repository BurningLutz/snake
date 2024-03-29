module Data.Game
  ( GameOver
  , gameOver
  , class MeatSpawner
  , spawnMeat

  , Game
  , new
  , next
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Data.Common (Point, between', (~))
import Data.Functor.Variant (SProxy(..))
import Data.GamingArea (GamingArea, HeightOutOfRange, WidthOutOfRange, dimension, gamingArea)
import Data.Snake (Direction, Snake, canBiteItself, canEat, grow, makeSnakeTowards, move)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj)
import Type.Row (type (+))

class Monad m <= MeatSpawner m where
  spawnMeat :: Int -> Int -> m Point

type GameOver r = (gameOver :: Unit | r)

gameOver :: forall r. Variant (GameOver + r)
gameOver = inj (SProxy :: SProxy "gameOver") unit

type Game =
  { gamingArea :: GamingArea
  , snake :: Snake
  , meat :: Point
  }

new
  :: forall r m
   . MeatSpawner m
  => Int -> Int -> Direction -> Point
  -> ExceptV (WidthOutOfRange + HeightOutOfRange + r) m Game
new w h dir p = do
  area <- gamingArea w h
  meat <- lift $ spawnMeat w h

  let
    snake = makeSnakeTowards dir p

  pure
    { gamingArea : area
    , snake
    , meat
    }

next
  :: forall r m
   . MeatSpawner m
  => Game
  -> ExceptV (GameOver + r) m Game
next { gamingArea, snake, meat } = do
  let
    w /\ h = dimension gamingArea

  snake' /\ meat' <- lift
    if snake `canEat` meat then do
      meat'' <- spawnMeat w h
      pure $ grow snake /\ meat''
    else
      pure $ move snake /\ meat

  let
    { head : x /\ y } = snake'

  if not (x `between'` (0 ~ w - 1) && y `between'` (0 ~ h - 1))
     || canBiteItself snake'
  then
    throwError gameOver
  else
    pure { gamingArea
         , snake : snake'
         , meat : meat'
         }
