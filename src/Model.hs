-- | This module contains the data types
--   which represent the state of the game
module Model where

import Assets
import Graphics.Gloss
import Gun
import Player
import System.Random

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

class Damagable a where
  damage :: a -> Int -> a

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
