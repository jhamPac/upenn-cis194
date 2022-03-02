module Main where

import           CodeWorld
import           Interaction
import           Maze
import           Traffic

main :: IO ()
main = startScreenInteractionOf initialCoord handleTime handleEvent drawState
