module Main where

import           CodeWorld

trafficCircle c p = colored c (translated 0 p (solidCircle 1))

frame = rectangle 2.5 5.5

trafficLight True = (trafficCircle green (-1.5)) & (trafficCircle black 1.5) & frame
trafficLight False = (trafficCircle black (-1.5)) & (trafficCircle red 1.5) & frame

main :: IO ()
main = drawingOf $ trafficLight False
