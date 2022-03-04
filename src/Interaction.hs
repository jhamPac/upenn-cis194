module Interaction where

import           CodeWorld
import           Maze

data Direction
    = R
    | U
    | L
    | D

data Coord = C Integer Integer deriving Eq

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) p = translated (fromIntegral x) (fromIntegral y) p

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

exampleCoord :: Coord
exampleCoord = adjacentCoord U (adjacentCoord U (adjacentCoord L initialCoord))

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up" = adjacentCoord U c
    | key == "Left" = adjacentCoord L c
    | key == "Down" = adjacentCoord D c
handleEvent _ c = c

drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze

data GameState world = StartScreen | Running world

startScreen :: Picture
startScreen = scaled 3 3 (text "Loading...")

data Interaction world = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)

withStartScreen :: Interaction s -> Interaction (GameState s)
withStartScreen (Interaction state step handle draw) = Interaction state' step' handle' draw'
    where
        state' = StartScreen
        step' _ StartScreen = StartScreen
        step' t (Running s) = Running (step t s)

        handle' (KeyPress key) StartScreen | key == " " = Running state
        handle' _ StartScreen = StartScreen
        handle' e (Running s) = Running (handle e s)

        draw' StartScreen = startScreen
        draw' (Running s) = draw s

resetable :: Interaction s -> Interaction s
resetable (Interaction state step handle draw) =
    Interaction state step handle' draw
    where
        handle' (KeyPress key) _ | key == "Esc" = state
        handle' e s = handle e s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state step handle draw) =
    interactionOf state step handle draw

data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state step handle draw) =
    Interaction state' step' handle' draw'
    where
        state' = WithUndo state []

        step' t (WithUndo s stack) = WithUndo (step t s) stack

        handle' (KeyPress key) (WithUndo s stack)
            | key == "U" = case stack of
                                (x:xs) -> WithUndo x xs
                                []     -> WithUndo s []

        handle' e (WithUndo s stack)
            | s' == s = WithUndo s stack
            | otherwise = WithUndo s' (s:stack)

             where
                s' = handle e s

        draw' (WithUndo s _) = draw s

