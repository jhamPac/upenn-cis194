module Main where

import           CodeWorld

greenCircle :: Picture
greenCircle = colored green $ solidCircle 1

main :: IO ()
main = drawingOf greenCircle
