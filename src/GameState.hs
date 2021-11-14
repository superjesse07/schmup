module GameState where

import Model
import Gun
import Player
import System.Random
import Assets
import Explosion

data GameState
  = MenuState
      { rng :: StdGen,
        assets :: Assets
      }
  | PlayingState
      { --elapsedTime :: Float,
        player :: Player,
        --turrets :: [Turret],
        --fighters :: [Fighter],
        --tanks :: [Tank],
        --cargoShips :: [CargoShip],
        explosions :: [Explosion],
        bullets :: [Projectile],
        --lasers :: [Laser],
        --playingScore :: Int,
        paused :: Bool,
        rng :: StdGen,
        assets :: Assets
      }
  | GameOverState
      { finalScore :: Int,
        highScores :: [Int],
        rng :: StdGen,
        assets :: Assets
      }