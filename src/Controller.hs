{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Controller where

import Data.List (delete)
import Data.Tuple.Extra (first, second)
import Graphics.Gloss.Interface.Pure.Game (
  Event (EventKey),
  Key (SpecialKey),
  KeyState (Down, Up),
  SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
 )

import Model (World (World, position, pressedKeys))

handleInput :: Event -> World -> World
handleInput event world@World {..} =
  case event of
    EventKey (SpecialKey k) action _ _ ->
      world
        { pressedKeys = case action of
            Down -> k : pressedKeys
            Up -> delete k pressedKeys
        }
    _ -> world

update :: Float -> World -> World
update _ world@World {..}
  | KeyDown `elem` pressedKeys = world {position = second (- 5) position}
  | KeyUp `elem` pressedKeys = world {position = second (+ 5) position}
  | KeyLeft `elem` pressedKeys = world {position = first (- 5) position}
  | KeyRight `elem` pressedKeys = world {position = first (+ 5) position}
  | otherwise = world
