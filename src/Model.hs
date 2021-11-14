-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import System.Random

data Animation = Animation
  { frame :: Int,
    animationTimer :: Float
  }

data LivingState = Living Int | Dying Float | Dead

class LivingObject a where
  isDead :: a -> Bool
  justDying :: a -> Bool
