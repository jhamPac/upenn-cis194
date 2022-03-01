module Interaction where

import           CodeWorld
import           Maze

data Direction
    = R
    | U
    | L
    | D

data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) p = translated (fromIntegral x) (fromIntegral y) p

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

exampleCoord :: Coord
exampleCoord = adjacentCoord U (adjacentCoord U (adjacentCoord L initialCoord))

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up" = adjacentCoord U c
    | key == "Left" = adjacentCoord L c
    | key == "Down" = adjacentCoord D c
handleEvent _ c = c

drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze
