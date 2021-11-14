module Player where

import Arith
import Assets
import Data.Default
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.Point.Arithmetic as Vector
import Graphics.Gloss.Interface.IO.Game
import Gun
import Consts 

-- player movement speed
playerMoveSpeed :: Float
playerMoveSpeed = 160.0

-- player dying time, how long it takes to die
playerDyingTime :: Float
playerDyingTime = 0.5

data PlayerState = Alive Int | Dying Float | Dead

data Player = Player
  { playerPosition :: Vector,
    playerState :: PlayerState,
    playerVelocity :: Vector,
    playerWeapon :: Gun
  }

-- shows the player
playerView :: Player -> Assets -> Picture
-- only show if we are alive
playerView player@Player {playerState = Alive _} assets = uncurry translate (playerPosition player) (playerSprite assets)
-- and not if we are dead
playerView _ _ = Blank

-- steps the player
playerStep :: Player -> Float -> Player
-- only move if we are alive TODO put this under movable
playerStep p@Player {playerState = Alive _} dt = p {playerPosition = pp Vector.+ ((dt Prelude.* playerSpeed) Vector.* pv)}
  where
    pv = playerVelocity p
    pp = playerPosition p
-- if we are dying, increase the time since death
playerStep p@Player {playerState = Dying t} dt = p {playerState = Dying (t Prelude.+ dt)}
-- if we are dead, do nothing
playerStep p _ = p

-- bit verbose but hey ho
playerInput :: Player -> Event -> Player
-- up
playerInput p (EventKey (SpecialKey KeyUp) Up _ _) = playerAddVelocity p (0.0, -1.0)
playerInput p (EventKey (SpecialKey KeyUp) Down _ _) = playerAddVelocity p (0.0, 1.0)
-- down
playerInput p (EventKey (SpecialKey KeyDown) Up _ _) = playerAddVelocity p (0.0, 1.0)
playerInput p (EventKey (SpecialKey KeyDown) Down _ _) = playerAddVelocity p (0.0, -1.0)
-- left
playerInput p (EventKey (SpecialKey KeyLeft) Up _ _) = playerAddVelocity p (1.0, 0.0)
playerInput p (EventKey (SpecialKey KeyLeft) Down _ _) = playerAddVelocity p (-1.0, 0.0)
-- right
playerInput p (EventKey (SpecialKey KeyRight) Up _ _) = playerAddVelocity p (-1.0, 0.0)
playerInput p (EventKey (SpecialKey KeyRight) Down _ _) = playerAddVelocity p (1.0, 0.0)
-- fire the weapon
playerInput p (EventKey (SpecialKey KeySpace) Down _ _) = fireGun p
-- other
playerInput p _ = p

-- implement the gun firing for the player
instance GunUser Player where
  fireGun p@Player {playerWeapon = gun} = p {playerWeapon = setGunFire (1.0, 0.0) gun}
  getGun p@Player {playerWeapon = gun} = gun
  stepGunUser p@Player {playerWeapon = gun} dt = (p {playerWeapon = newGun}, newProjectile)
    where
      (newGun, newProjectile) = stepGun PlayerOwner (playerPosition p Vector.+ (7,0.5)) gun dt

-- helper function to move
playerAddVelocity :: Player -> Vector -> Player
playerAddVelocity p v = p {playerVelocity = v Vector.+ playerVelocity p}

instance Default Player where
  def = Player (0, 0) (Alive 5) (0, 0) (getDefaultGun DefaultType)
