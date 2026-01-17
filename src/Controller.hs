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

import Model (
  CharacterStatus (..),
  GlobalState (..),
  Object (Object, position),
  Screen (..),
  UiState (..),
  World (..),
  initialWorld,
 )

handleInput :: Event -> GlobalState -> GlobalState
handleInput event state@GlobalState {..} =
  case event of
    EventKey (SpecialKey k) action _ _ ->
      state
        { uiState =
            uiState
              { pressedKeys = case action of
                  Down -> k : pressedKeys uiState
                  Up -> delete k $ pressedKeys uiState
              }
        }
    _ -> state

moveSpeed, floatSpeed, fallSpeed :: Float
moveSpeed = 300
floatSpeed = 60
fallSpeed = 200

update :: Float -> GlobalState -> GlobalState
update t state@GlobalState {..} =
  state
    { screen = case screen of
        StartScreen -> GameScreen initialWorld
        GameScreen world -> GameScreen $ updateWorld t uiState world
        HighScoreScreen -> StartScreen
    }

updateWorld :: Float -> UiState -> World -> World
updateWorld t UiState {..} world@World {character = me@(Object (x, y) _), ..} =
  world
    { character =
        me
          { position =
              ( x + moveSpeed * t * modifier,
                case characterStatus of
                  CharacterInBalloon -> y + 2 * t * floatSpeed
                  CharacterInBubble -> y + t * floatSpeed
                  PlainCharacter ->  y - t * fallSpeed
              )
          }
    }
  where
    modifier
      | KeyLeft `elem` pressedKeys = -1
      | KeyRight `elem` pressedKeys = 1
      | otherwise = 0
