{-# LANGUAGE LexicalNegation #-}

module Main where

import Graphics.Gloss
import Controller
import Model
import View

initialWindowPosition :: (Int, Int)
initialWindowPosition = (10, 10)

loadSprites :: IO Assets
loadSprites = do
  player <- loadBMP "./assets/sprite.bmp"
  pure
    Assets
      { player = player,
        bubble = circleSolid 30
      }

main :: IO ()
main =
  do
    assets <- loadSprites
    let initializedWorld = initialWorld assets in
      play
        (InWindow "GlossyGaming" (windowSize initializedWorld) initialWindowPosition)
        green
        60
        initializedWorld
        render
        handleInput
        update
