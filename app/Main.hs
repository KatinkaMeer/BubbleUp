{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (delete)
import Data.Tuple.Extra (first, second)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data World = World
  { position :: (Float, Float),
    pressedKeys :: [SpecialKey]
  }

main :: IO ()
main =
  play
    (InWindow "GlossyGaming" (500, 500) (10, 10))
    white
    60
    initialWorld
    render
    handleInput
    update

initialWorld :: World
initialWorld = World (0, 0) []

render :: World -> Picture
render World{..} =
  uncurry translate position
    $ color black
    $ circleSolid 25

handleInput :: Event -> World -> World
handleInput event world@World{..} =
  case event of
    EventKey (SpecialKey k) action _ _ ->
      world
        { pressedKeys = case action of
            Down -> k : pressedKeys
            Up -> delete k pressedKeys
        }
    _ -> world

update :: Float -> World -> World
update _ world@World{..}
  | KeyDown `elem` pressedKeys = world{position = second (- 5) position}
  | KeyUp `elem` pressedKeys = world{position = second (+ 5) position}
  | KeyLeft `elem` pressedKeys = world{position = first (- 5) position}
  | KeyRight `elem` pressedKeys = world{position = first (+ 5) position}
  | otherwise = world
