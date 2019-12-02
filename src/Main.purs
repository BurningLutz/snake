module Main where 

import Prelude

import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Game (class SpawnMeat, Game, GameOver, newGame, next)
import Data.GamingArea (HeightOutOfRange, WidthOutOfRange)
import Data.Int (toNumber)
import Data.Snake (Direction(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Ref (Ref, new, read, write)
import Type.Row (type (+))

newtype AppM a = AppM (ReaderT Unit Aff a)

derive newtype instance monadAppM :: Monad AppM 
derive newtype instance bindAppM :: Bind AppM 
derive newtype instance applicativeAppM :: Applicative AppM 
derive newtype instance applyAppM :: Apply AppM 
derive newtype instance functorAppM :: Functor AppM 
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

class Monad m <= MutableGame m where
  newGameRef :: Game -> m (Ref Game)
  readGame :: Ref Game -> m Game
  writeGame :: Game -> Ref Game -> m Unit

class Monad m <= Sleepable m where
  sleep :: Int -> m Unit

instance spawnMeatAppM :: SpawnMeat AppM where
  spawnMeat w h = pure (50 /\ 30)

instance mutableGameAppM :: MutableGame AppM where
  newGameRef = liftEffect <<< new
  readGame = liftEffect <<< read
  writeGame g = liftEffect <<< write g

instance sleepableAppM :: Sleepable AppM where
  sleep i = liftAff $ delay (Milliseconds $ toNumber i)

runAppM :: AppM ~> Aff
runAppM (AppM m) = runReaderT m unit

appM
  :: forall r m
   . SpawnMeat m
  => MutableGame m
  => Sleepable m
  => ExceptV
       ( WidthOutOfRange
       + HeightOutOfRange
       + GameOver
       + r
       )
       m Unit
appM = do
  game <- newGame 80 50 D (40 /\ 25)

  gameRef <- lift $ newGameRef game

  let
    tick = do
      currentGame <- lift $ readGame gameRef
      nextGame <- next currentGame

      lift $ writeGame nextGame gameRef

      lift $ sleep 100

      tick

  tick

main :: Effect Unit
main = do
  launchAff_ $ runAppM $ safe $ appM # handleError
    { widthOutOfRange  : \s -> logShow s
    , heightOutOfRange : \s -> logShow s
    , gameOver         : \_ -> logShow "Game Over"
    }
