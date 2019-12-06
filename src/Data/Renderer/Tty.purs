module Data.Renderer.Tty
  ( render
  ) where

import Prelude

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Array (modifyAtIndices, replicate, updateAtIndices)
import Data.Common (Point)
import Data.Foldable (foldl)
import Data.Game (Game)
import Data.GamingArea (dimension)
import Data.Snake (Direction, Head, Snake, toVector)
import Data.String (joinWith)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.Process (stdout)
import Node.Stream (writeString)

type GameMap = Array (Array String)

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

writeTtyString :: String -> Effect Unit -> Effect Unit
writeTtyString s cb = void $ writeString stdout UTF8 s cb

render :: Game -> Effect Unit -> Effect Unit
render { gamingArea, meat, snake } cb = do
  let
    w /\ h = dimension gamingArea
    areaMap = area w h
      # placeMeatAt meat
      # placeSnake snake

    outputString =
      escapeCodeToString (Position 0 0)
      <> ( areaMap
         # map (joinWith "" >>> (_ <> escapeCodeToString $ NextLine 1))
         # joinWith ""
         )

  writeTtyString outputString cb
