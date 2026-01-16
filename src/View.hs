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

import Model (World (World, position))

render :: World -> Picture
render World {position = (x, y), ..} =
  pictures
    $
    -- player sprite
    color black (circleSolid 25)
      :
      -- other stuff in the scene
      map
        (translate (-x) (-y))
        [ translate 80 40 $ circleSolid 30,
          translate (-250) 0 $ rectangleSolid 100 1000
        ]
