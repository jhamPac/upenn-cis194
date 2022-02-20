module Main where

import           CodeWorld

circles :: Picture
circles = colored green (solidCircle 1) & solidCircle 2

main :: IO ()
main = drawingOf circles
