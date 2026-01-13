module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type World = (Float, Float)

main :: IO ()
main = play
    (InWindow "GlossyGaming" (500, 500) (10, 10))
    white
    60
    initialWorld
    render
    handleInput
    update


initialWorld :: World
initialWorld = (0,0)

render :: World -> Picture
render (x,y) =
    translate x y $
        color black $
            circleSolid 25

handleInput :: Event -> World -> World
handleInput event (x,y) =
    case event of
        EventKey (SpecialKey KeyDown) Down _ _ -> (x, y-20)
        EventKey (SpecialKey KeyUp) Down _ _ -> (x, y+20)
        EventKey (SpecialKey KeyLeft) Down _ _ -> (x-20, y)
        EventKey (SpecialKey KeyRight) Down _ _ -> (x+20, y)
        _ -> (x, y)

update :: Float -> World -> World
update _ world =
    world 