module State where

import Player
import Gun
import Assets
import System.Random
import Model

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
        --explosions :: [Explosion],
        bullets :: [Projectile],
        --lasers :: [Laser],
        --playingScore :: Int,
        paused :: Bool,
        assets :: Assets
      }
  | GameOverState
      { finalScore :: Int,
        highScores :: [Int],
        assets :: Assets
      }