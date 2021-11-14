-- | This module contains the data types
--   which represent the state of the game
module Model where

import Assets
import Graphics.Gloss
import Gun
import Player
import System.Random

data Animation = Animation
  { frame :: Int,
    animationTimer :: Float
  }

data LivingState = Living Int | Dying Float | Dead

