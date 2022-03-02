module Main where

import           CodeWorld
import           Interaction
import           Maze
import           Traffic

main :: IO ()
main = runInteraction $ resetable $ withStartScreen (Interaction initialCoord handleTime handleEvent drawState)
