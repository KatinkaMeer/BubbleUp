{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Graphics.Gloss (
  Picture,
  black,
  blank,
  circleSolid,
  color,
  pictures,
  rectangleSolid,
  translate,
 )

import Model (
  Assets (Assets, player),
  GlobalState (..),
  Object (Object, position),
  Screen (..),
  UiState (UiState, assets),
  World (World, character),
 )

render :: GlobalState -> Picture
render GlobalState {..} = case screen of
  StartScreen -> blank
  GameScreen world -> renderWorld (assets uiState) world
  HighScoreScreen -> blank

renderWorld :: Assets -> World -> Picture
renderWorld
  Assets {..}
  World
    { character = Object {position = (x, y)},
      ..
    } =
    pictures
      $
      -- player sprite
      player
        :
        -- other stuff in the scene
        map
          (translate (-x) (-y))
          [ translate 80 40 $ circleSolid 30,
            translate (-250) 0 $ rectangleSolid 100 1000
          ]
