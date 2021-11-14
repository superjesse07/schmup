module GameState where

import Player
import Gun
import Assets
import Turret
import Background
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
        background :: [BackgroundObject],
        playingScore :: Int,
        paused :: Bool,
        assets :: Assets
      }
  | GameOverState
      { finalScore :: Int,
        highScores :: [Int],
        assets :: Assets
      }