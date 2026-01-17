{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Graphics.Gloss (
  Picture (Pictures),
  black,
  circleSolid,
  color,
  pictures,
  rectangleSolid,
  translate,
 )
import View.Frog (
  FrogState (FrogState, eyesOpen, mouthOpen),
  frogSprite,
 )

import Model (
  Assets (..),
  Object (Object, position),
  World (..),
  objectDataToPicture,
 )

render :: World -> Picture
render
  World
    { character = Object {position = (x, y)},
      assets = assets@Assets {player = playerSprite, ..},
      ..
    } =
    pictures
      $
      -- player sprite
      bubble
        : frogSprite assets FrogState {eyesOpen = True, mouthOpen = False}
        :
        -- other stuff in the scene
        map
          (translate (-x) (-y))
          ( translate (-250) 0 (rectangleSolid 100 1000)
              : map (objectDataToPicture assets) objects
          )
