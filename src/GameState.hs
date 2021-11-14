module GameState where

import Player
import Gun
import Assets
import Turret
import Background
import Explosion
import Fighter
import Cargo

data GameState
  = MenuState
      { assets :: Assets,
        screenSize :: (Int, Int)
      }
  | PlayingState
      { 
        player :: Player,
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