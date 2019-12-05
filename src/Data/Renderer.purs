module Data.Renderer
  ( class MonadTtyStream
  , writeTtyString
  , writeTtyEscapeCode

  , renderGame
  ) where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Data.Array (modifyAtIndices, replicate, updateAtIndices)
import Data.Common (Point)
import Data.Foldable (foldl)
import Data.Game (Game)
import Data.GamingArea (dimension)
import Data.Snake (Direction, Head, Snake, toVector)
import Data.String (joinWith)
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))

type GameMap = Array (Array String)

class Monad m <= MonadTtyStream m where
  writeTtyString :: String -> m Unit
  writeTtyEscapeCode :: EscapeCode -> m Unit

area :: Int -> Int -> GameMap
area w h = replicate w "." # replicate h

placeObjectAt :: String -> Point -> GameMap -> GameMap
placeObjectAt o (x /\ y) = modifyAtIndices [y] (updateAtIndices [x /\ o])

placeMeatAt :: Point -> GameMap -> GameMap
placeMeatAt = placeObjectAt "O"

placeSnake :: Snake -> GameMap -> GameMap
placeSnake { head, body } = placeObjectAt "@" head >>> placeBody
  where
    placeBody :: GameMap -> GameMap
    placeBody gm = snd $ foldl placeBodyPart (head /\ gm) body

    placeBodyPart :: Head /\ GameMap -> Direction -> Head /\ GameMap
    placeBodyPart (h /\ gm) dir =
      let
        h'  = h - toVector dir
        gm' = placeObjectAt "#" h' gm

      in h' /\ gm'

renderGame
  :: forall m
   . MonadTtyStream m
  => Game
  -> m Unit
renderGame { gamingArea, meat, snake } = do
  let
    w /\ h = dimension gamingArea
    areaMap = area w h
      # placeMeatAt meat
      # placeSnake snake

  writeTtyEscapeCode $ Position 0 0

  areaMap # traverse_ \chars -> do
    writeTtyString $ joinWith "" chars
    writeTtyEscapeCode $ NextLine 1
