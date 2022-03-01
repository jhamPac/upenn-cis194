module Maze where

import           CodeWorld

wall, ground, storage, box :: Picture

wall = colored (grey 0.4) (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box = colored brown (solidRectange 1 1)


