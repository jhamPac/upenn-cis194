module Traffic where

trafficCircle c p = colored c (translated 0 p (solidCircle 1))
frame = rectangle 2.5 5.5

trafficLight True = (trafficCircle green (-1.5)) & (trafficCircle black 1.5) & frame
trafficLight False = (trafficCircle black (-1.5)) & (trafficCircle red 1.5) & frame

trafficController t
    | round (t/3) `mod` 2 == 0 = trafficLight True
    | otherwise = trafficLight False
