module Data.GamingArea
  ( GamingArea
  , WidthOutOfRange
  , HeightOutOfRange
  , gamingArea
  , widthOutOfRange
  , heightOutOfRange
  , dimension
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Functor.Variant (SProxy(..))
import Data.Variant (Variant, inj)
import Type.Row (type (+))
import Data.Tuple.Nested (type (/\), (/\))

type WidthOutOfRange r = (widthOutOfRange :: String | r)
type HeightOutOfRange r = (heightOutOfRange :: String | r)

-- | a variant instance of WidthOutOfRange
widthOutOfRange :: forall r. Variant (WidthOutOfRange + r)
widthOutOfRange = inj (SProxy :: SProxy "widthOutOfRange") "width should be between 2 and 100"

-- | a variant instance of HeightOutOfRange
heightOutOfRange :: forall r. Variant (HeightOutOfRange + r)
heightOutOfRange = inj (SProxy :: SProxy "heightOutOfRange") "height should be between 2 and 100"

data GamingArea = GamingArea Int Int

-- | safely make a gaming area by validating width and height in advance
gamingArea
  :: forall r m
   . Monad m
  => Int -> Int
  -> ExceptV (WidthOutOfRange + HeightOutOfRange + r) m GamingArea
gamingArea w h | w < 2 || w > 100 = throwError widthOutOfRange
               | h < 2 || h > 100 = throwError heightOutOfRange
               | otherwise        = pure (GamingArea w h)

dimension :: GamingArea -> Int /\ Int
dimension (GamingArea w h) = w /\ h
