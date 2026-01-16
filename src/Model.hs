module Model (
  World (..),
  initialWorld,
) where

import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data World = World
  { position :: !(Float, Float),
    pressedKeys :: ![SpecialKey]
  }

initialWorld :: World
initialWorld = World (0, 0) []
