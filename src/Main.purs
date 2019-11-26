module Main where

import Prelude

import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Data.GamingArea (HeightOutOfRange, WidthOutOfRange, gamingArea)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Type.Row (type (+))

logGamingArea
  :: forall r m
   . MonadEffect m
  => Int -> Int
  -> ExceptV (WidthOutOfRange + HeightOutOfRange + r) m Unit
logGamingArea w h = do
  area <- gamingArea w h
  logShow area

main :: Effect Unit 
main = do
  let
    handleAll = handleError
      { widthOutOfRange : \s -> logShow s
      , heightOutOfRange : \s -> logShow s
      }

  safe do
    logGamingArea 0 20   # handleAll
    logGamingArea 10 200 # handleAll
    logGamingArea 20 30  # handleAll
