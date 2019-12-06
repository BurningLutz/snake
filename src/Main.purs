module Main where 

import Prelude

import Control.Apply (lift2)
import Control.Monad.Except.Checked (handleError, safe)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.AppM (class GameProvider, class Interactive, class Quitable, class Setup, class Suspendable, appM, onKey, quit)
import Data.Game (class MeatSpawner)
import Data.Renderer.Class (class Renderer)
import Data.Renderer.Tty as Tty
import Data.Tuple.Nested ((/\))
import Data.Util (setRawMode)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.Process (exit, stdin)
import Node.Stream (onDataString)

quit' :: Effect Unit 
quit' = void $ exit 0

newtype AppM a = AppM (ReaderT Unit Aff a)

derive newtype instance monadAppM       :: Monad AppM
derive newtype instance bindAppM        :: Bind AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance applyAppM       :: Apply AppM
derive newtype instance functorAppM     :: Functor AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM    :: MonadAff AppM

instance meatSpawnerAppM :: MeatSpawner AppM where
  spawnMeat w h = liftEffect $ lift2 (/\) (randomInt 0 (w - 1)) (randomInt 0 (h - 1))

instance gameProviderAppM :: GameProvider AppM where
  new = liftEffect <<< Ref.new
  read = liftEffect <<< Ref.read
  write r = liftEffect <<< flip Ref.write r

instance suspendableAppM :: Suspendable AppM where
  suspend = liftAff $ delay (Milliseconds 100.0)

instance rendererAppM :: Renderer AppM where
  render s = liftAff $ makeAff \cb -> do
    -- here we call the `cb $ pure unit` to just workaround the either
    void $ Tty.render s (cb $ pure unit)
    pure nonCanceler

instance interactiveAppM :: Interactive AppM where
  onKey k p = liftEffect $ onDataString stdin UTF8 \s -> do
    when (k == s) (launchAff_ $ runAppM p)

instance quitableAppM :: Quitable AppM where
  quit = liftEffect quit'

instance setupAppM :: Setup AppM where
  setup = do
    liftEffect $ setRawMode stdin true
    -- under raw mode, we should manually handle Ctrl-C, the \3
    onKey "\x03" quit

runAppM :: AppM ~> Aff
runAppM (AppM m) = runReaderT m unit

main :: Effect Unit
main = do
  launchAff_ $ runAppM $ safe $ appM # handleError
    { widthOutOfRange  : \s -> logShow s
    , heightOutOfRange : \s -> logShow s
    , gameOver         : \_ -> do
                           logShow "Game Over"
                           liftEffect quit'
    }
