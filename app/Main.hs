module Main where

import           CodeWorld

screen :: Picture
screen = solidCircle 1

main :: IO ()
main = drawingOf screen
