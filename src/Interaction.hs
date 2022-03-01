module Interaction where

import           CodeWorld

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
