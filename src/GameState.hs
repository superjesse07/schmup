module GameState where

import Player
import Gun
import Assets
import Turret
import Explosion

data GameState
  = MenuState
      { assets :: Assets,
        screenSize :: (Int, Int)
      }
  | PlayingState
      { --elapsedTime :: Float,
        player :: Player,
        turrets :: [Turret],
        --fighters :: [Fighter],
        --tanks :: [Tank],
        --cargoShips :: [CargoShip],
        explosions :: [Explosion],
        bullets :: [Projectile],
        playingScore :: Int,
        paused :: Bool,
        assets :: Assets,
        screenSize :: (Int, Int)
      }
  | GameOverState
      { finalScore :: Int,
        highScores :: [Int],
        assets :: Assets,
        screenSize :: (Int, Int)
      }