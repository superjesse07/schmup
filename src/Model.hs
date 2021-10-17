-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Data.Vector (Vector)

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState
  = MenuState
  | PlayingState
      { elapsedTime :: Float,
        player :: Player,
        turrets :: [Turret],
        fighters :: [Fighter],
        tanks :: [Tank],
        cargoShips :: [CargoShip],
        explosions :: [Explosion],
        bullets :: [Bullet],
        lasers :: [Laser],
        playingScore :: Int,
        paused :: Bool
      }
  | GameOverState
      { finalScore :: Int
      }

data Gun = LaserGun | DefaultGun | BurstGun

data OwnerShip = PlayerOwner | EnemyOwner

data PlayerState = Alive | Dying Float | Dead


class Damagable a where
  damage :: a -> a

class Moveable a where
  move :: a -> a

data Animation = Animation
  { frame :: Int,
    animationTimer :: Float
  }

data Explosion = Explosion
  { explosionAnimation :: Animation,
    explosionsLeft :: Int
  }

data Player = Player
  { playerPosition :: Vector,
    playerHealth :: Int,
    playerState :: PlayerState,
    playerVelocity :: Vector,
    playerGun :: Gun,
    playerAnimation :: Animation
  }

data Bullet = Bullet
  { bulletPosition :: Vector,
    bulletVelocity :: Vector,
    bulletOwner :: OwnerShip
  }

data Laser = Laser
  { laserPosition :: Vector,
    laserOwner :: OwnerShip,
    laserAnimation :: Animation
  }

data Turret = Turret
  { turretPosition :: Vector,
    turretHealth :: Int
  }

data Fighter = Fighter
  { fighterPosition :: Vector,
    fighterHealth :: Int,
    fighterVelocity :: Vector,
    fighterAnimation :: Animation
  }

data Tank = Tank
  { tankPosition :: Vector,
    tankHealth :: Int,
    tankVelocity :: Vector,
    tankAnimation :: Animation
  }

data CargoShip = CargoShip
  { cargoShipPosition :: Vector,
    cargoShipHealth :: Int,
    cargoShipVelocity :: Vector,
    cargoShipAnimation :: Animation
  }