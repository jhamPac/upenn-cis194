module Main where

import           CodeWorld
import           Interaction
import           Maze
import           Traffic

main :: IO ()
main = interactionOf initialCoord handleTime handleEvent drawState
