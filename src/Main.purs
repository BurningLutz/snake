module Main where 

import Prelude

import Ansi.Codes (escapeCodeToString)
import Control.Apply (lift2)
import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Variant (SProxy(..))
import Data.Game (class SpawnMeat, Game, GameOver, newGame, next)
import Data.GamingArea (HeightOutOfRange, WidthOutOfRange)
import Data.Int (toNumber)
import Data.Renderer (class MonadTtyStream, renderGame, writeTtyString)
import Data.Snake (Direction(..), towards)
import Data.Tuple.Nested ((/\))
import Data.Util (setRawMode)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Random (randomInt)
import Effect.Ref (Ref, new, read, write)
import Node.Encoding (Encoding(..))
import Node.Process (exit, stdin, stdout)
import Node.Stream (onDataString, writeString)
import Record (modify)
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

class Monad m <= Interactive m where
  onKey :: String -> m Unit -> m Unit

class Monad m <= Quitable m where
  quit :: m Unit

instance spawnMeatAppM :: SpawnMeat AppM where
  spawnMeat w h = liftEffect $ lift2 (/\) (randomInt 0 w) (randomInt 0 h)

instance mutableGameAppM :: MutableGame AppM where
  newGameRef = liftEffect <<< new
  readGame = liftEffect <<< read
  writeGame g = liftEffect <<< write g

instance sleepableAppM :: Sleepable AppM where
  sleep i = liftAff $ delay (Milliseconds $ toNumber i)

instance monadTtyStreamAppM :: MonadTtyStream AppM where
  writeTtyString s = liftAff $ makeAff \cb -> do
    -- here we call the `cb $ pure unit` to just workaround the either
    void $ writeString stdout UTF8 s (cb $ pure unit)
    pure nonCanceler
  writeTtyEscapeCode esc = writeTtyString $ escapeCodeToString esc

instance interactiveAppM :: Interactive AppM where
  onKey k p = liftEffect $ onDataString stdin UTF8 \s -> do
    if k == s then
      launchAff_ $ runAppM p
    else
      pure unit

instance quitAppM :: Quitable AppM where
  quit = liftEffect $ void $ exit 0

runAppM :: AppM ~> Aff
runAppM (AppM m) = runReaderT m unit

appM
  :: forall r m
   . SpawnMeat m
  => MutableGame m
  => MonadTtyStream m
  => Sleepable m
  => Interactive m
  => Quitable m
  => ExceptV
       ( WidthOutOfRange
       + HeightOutOfRange
       + GameOver
       + r
       )
       m Unit
appM = do
  game <- newGame 80 30 D (40 /\ 25)

  gameRef <- lift $ newGameRef game

  let
    towards' :: Direction -> m Unit
    towards' dir = do
      readGame gameRef
        <#> modify (SProxy :: SProxy "snake") (towards dir)
        >>= flip writeGame gameRef

  -- manually handle Ctrl-C, the \3
  lift $ onKey "\x03" quit

  -- key bindings
  lift $ onKey "w" (towards' W)
  lift $ onKey "s" (towards' S)
  lift $ onKey "a" (towards' A)
  lift $ onKey "d" (towards' D)

  let
    tick = do
      currentGame <- lift $ readGame gameRef

      lift $ renderGame currentGame

      nextGame <- next currentGame

      lift $ writeGame nextGame gameRef

      lift $ sleep 100

      tick

  tick

main :: Effect Unit
main = do
  setRawMode stdin true

  launchAff_ $ runAppM $ safe $ appM # handleError
    { widthOutOfRange  : \s -> logShow s
    , heightOutOfRange : \s -> logShow s
    , gameOver         : \_ -> do
                           logShow "Game Over"
                           liftEffect $ void $ exit 0
    }
