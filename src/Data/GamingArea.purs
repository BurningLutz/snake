module Data.GamingArea
  ( GamingArea
  , WidthOutOfRange
  , HeightOutOfRange
  , gamingArea
  , widthOutOfRange
  , heightOutOfRange
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Functor.Variant (SProxy(..))
import Data.Identity (Identity)
import Data.Variant (Variant, inj)
import Type.Row (type (+))

type WidthOutOfRange r = (widthOutOfRange :: String | r)
type HeightOutOfRange r = (heightOutOfRange :: String | r)

widthOutOfRange :: forall r. Variant (WidthOutOfRange + r)
widthOutOfRange = inj (SProxy :: SProxy "widthOutOfRange") "width should be between 2 and 100"

heightOutOfRange :: forall r. Variant (HeightOutOfRange + r)
heightOutOfRange = inj (SProxy :: SProxy "heightOutOfRange") "height should be between 2 and 100"

data GamingArea = GamingArea Int Int

gamingArea :: forall r. Int -> Int -> ExceptV (WidthOutOfRange + HeightOutOfRange + r) Identity GamingArea
gamingArea w h | w < 2 || w > 100 = throwError widthOutOfRange
               | h < 2 || h > 100 = throwError heightOutOfRange
               | otherwise        = pure (GamingArea w h)
