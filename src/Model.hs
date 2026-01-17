module Model (
  CharacterStatus (..),
  GlobalState (..),
  Jump (..),
  Object (..),
  Screen (..),
  UiState (..),
  World (..),
  Assets (..),
  initialGlobalState,
  initialWorld,
)
where

import Graphics.Gloss (Picture, Point, Vector)
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data GlobalState = GlobalState
  { uiState :: !UiState,
    screen :: !Screen
  }

initialGlobalState
  :: Assets
  -- ^ Pre-rendered pictures
  -> GlobalState
initialGlobalState assets =
  GlobalState
    { uiState =
        UiState
          { assets = assets,
            highScores = HighScores [],
            pressedKeys = [],
            windowSize = (800, 450)
          },
      screen = StartScreen
    }

data UiState = UiState
  { assets :: !Assets,
    highScores :: !HighScores,
    pressedKeys :: ![SpecialKey],
    windowSize :: !Vector
  }

data Screen
  = StartScreen
  | GameScreen !World
  | HighScoreScreen

newtype HighScores = HighScores
  { unHighScores :: [(String, Integer)]
  }

data Object = Object
  { position :: !Point,
    velocity :: !Vector
  }

data ObjectType = Balloon | Bubble

data Jump = Jump
  { direction :: !Vector,
    speed :: !Float
  }

data CharacterStatus
  = CharacterInBalloon
  | CharacterInBubble
  | PlainCharacter

data Assets = Assets
  { player :: !Picture,
    bubble :: !Picture
  }

data World = World
  { character :: !Object,
    characterStatus :: !CharacterStatus,
    elapsedTime :: !Float,
    viewport :: !Object,
    jump :: !(Maybe Jump),
    objects :: ![(ObjectType, Object)]
  }

initialWorld :: World
initialWorld =
  World
    { character = Object (0, 0) (0, 0),
      characterStatus = CharacterInBubble,
      elapsedTime = 0,
      viewport = Object (0, 0) (0, 0),
      jump = Nothing,
      objects = []
    }
