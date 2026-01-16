{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Graphics.Gloss (
  Picture,
  black,
  circleSolid,
  color,
  translate,
 )

import Model (World (World, position))

render :: World -> Picture
render World {..} =
  uncurry translate position
    $ color black
    $ circleSolid 25
