module GameState where

import Player
import Gun
import Assets
import Turret
import Explosion

data GameState
  = MenuState
      { assets :: Assets
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
        assets :: Assets
      }
  | GameOverState
      { finalScore :: Int,
        highScores :: [Int],
        assets :: Assets
      }