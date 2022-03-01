module Maze (pictureOfMaze) where

import           CodeWorld

wall, ground, storage, box :: Picture

wall = colored (grey 0.4) (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box = colored brown (solidRectangle 1 1)

data Tile
    = Wall
    | Ground
    | Storage
    | Box
    | Blank

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21Times (\r -> draw21Times (\c -> drawTileAt r c))

draw21Times :: (Integer -> Picture) -> Picture
draw21Times f = go (-10)
    where
        go :: Integer -> Picture
        go 11 = blank
        go n  = f n & go (n + 1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))

maze :: Integer -> Integer -> Tile
maze x y
    | abs x > 4 || abs y > 4 = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x == 2 && y <= 0 = Wall
    | x == 3 && y <= 0 = Storage
    | x >= -2 && y == 0 = Box
    | otherwise = Ground
