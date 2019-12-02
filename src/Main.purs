module Main where

import Prelude

import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Game (class SpawnMeat, newGame, next)
import Data.GamingArea (HeightOutOfRange, WidthOutOfRange)
import Data.Snake (Direction(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Ref (new, read, write)
import Effect.Timer (setTimeout)
import Type.Row (type (+))

newtype AppM a = AppM (ReaderT Unit Effect a)

derive newtype instance monadAppM :: Monad AppM 
derive newtype instance bindAppM :: Bind AppM 
derive newtype instance applicativeAppM :: Applicative AppM 
derive newtype instance applyAppM :: Apply AppM 
derive newtype instance functorAppM :: Functor AppM 
derive newtype instance monadEffectAppM :: MonadEffect AppM

instance spawnMeatAppM :: SpawnMeat AppM where
  spawnMeat w h = pure (50 /\ 30)

runAppM :: AppM ~> Effect
runAppM (AppM m) = runReaderT m unit

runTick :: Effect Unit -> Effect Unit 
runTick h = void $ setTimeout 100 h

appM
  :: forall r
   . ExceptV
       ( WidthOutOfRange
       + HeightOutOfRange
       + r
       )
       AppM Unit
appM = do
  game <- newGame 80 50 D (40 /\ 25)
  gameRef <- liftEffect $ new game

  let
    tick = do
      currentGame <- read gameRef

      let p = do
            nextGame <- next currentGame
            liftEffect $ write nextGame gameRef
            liftEffect $ runTick tick

      runAppM $ safe $ p # handleError
        { gameOver : \_ -> do
            logShow "Game Over"
        }

  liftEffect $ runTick tick

main :: Effect Unit
main = do
  runAppM $ safe $ appM # handleError
    { widthOutOfRange  : \s -> logShow s
    , heightOutOfRange : \s -> logShow s
    }
