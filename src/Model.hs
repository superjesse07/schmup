-- | This module contains the data types
--   which represent the state of the game
module Model where
import           Assets
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector (Vector)
import           Player
import           System.Random

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

data GameState
  = MenuState { rng :: StdGen,
        assets      :: Assets
      }
  | PlayingState
      { --elapsedTime :: Float,
        player :: Player,
        --turrets :: [Turret],
        --fighters :: [Fighter],
        --tanks :: [Tank],
        --cargoShips :: [CargoShip],
        --explosions :: [Explosion],
        --bullets :: [Bullet],
        --lasers :: [Laser],
        --playingScore :: Int,
        --paused :: Bool,
        rng    :: StdGen,
        assets :: Assets
      }
  | GameOverState
      { finalScore :: Int,
        rng        :: StdGen,
        assets     :: Assets
      }

data Gun = LaserGun | DefaultGun | BurstGun

data OwnerShip = PlayerOwner | EnemyOwner


class Damagable a where
  damage :: a -> a

class Moveable a where
  move :: a -> a

data Animation = Animation
  { frame          :: Int,
    animationTimer :: Float
  }

data Explosion = Explosion
  { explosionAnimation :: Animation,
    explosionsLeft     :: Int
  }

data Bullet = Bullet
  { bulletPosition :: Vector,
    bulletVelocity :: Vector,
    bulletOwner    :: OwnerShip
  }

data Laser = Laser
  { laserPosition  :: Vector,
    laserOwner     :: OwnerShip,
    laserAnimation :: Animation
  }

data Turret = Turret
  { turretPosition :: Vector,
    turretHealth   :: Int
  }

data Fighter = Fighter
  { fighterPosition  :: Vector,
    fighterHealth    :: Int,
    fighterVelocity  :: Vector,
    fighterAnimation :: Animation
  }

data Tank = Tank
  { tankPosition  :: Vector,
    tankHealth    :: Int,
    tankVelocity  :: Vector,
    tankAnimation :: Animation
  }

data CargoShip = CargoShip
  { cargoShipPosition  :: Vector,
    cargoShipHealth    :: Int,
    cargoShipVelocity  :: Vector,
    cargoShipAnimation :: Animation
  }
