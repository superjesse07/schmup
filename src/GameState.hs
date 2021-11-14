module GameState where

import Assets
import Background
import Cargo
import Explosion
import Fighter
import Gun
import Player
import Turret

data GameState
  = MenuState
      { assets :: Assets,
        screenSize :: (Int, Int)
      }
  | PlayingState
      { player :: Player,
        turrets :: [Turret],
        fighters :: [Fighter],
        cargoShips :: [CargoShip],
        cargoDrops :: [CargoPickup],
        explosions :: [Explosion],
        bullets :: [Projectile],
        background :: [BackgroundObject],
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
