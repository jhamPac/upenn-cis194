module Main where

import           CodeWorld

greenLight :: Picture
greenLight = colored green (translated 0 (-1.5) (solidCircle 1))

redLight :: Picture
redLight = colored red (translated 0 (1.5) (solidCircle 1))

frame = rectangle 2.5 5.5

trafficLight = greenLight & redLight & frame

main :: IO ()
main = drawingOf trafficLight
