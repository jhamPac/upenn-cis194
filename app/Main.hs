module Main where

import           CodeWorld
import           Traffic

main :: IO ()
main = animationOf trafficController
