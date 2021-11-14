module Player where

import Arith
import Assets
import Consts
import Data.Default
import Debug.Trace
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vector
import Graphics.Gloss.Interface.IO.Game
import Gun
import Model
import GHC.Float.RealFracMethods (int2Float)

-- player movement speed
playerMoveSpeed :: Float
playerMoveSpeed = 160.0

-- player dying time, how long it takes to die
playerDyingTime :: Float
playerDyingTime = 0.5

data Player = Player
  { playerPosition :: Vector,
    playerState :: LivingState,
    playerVelocity :: Vector,
    playerWeapon :: Gun,
    playerHitTimer :: Float,
    playerPowerup :: Float
  }

-- shows the player
playerView :: Player -> Assets -> Picture
-- only show if we are alive
playerView player@Player {playerState = Living _, playerHitTimer = timer} assets
  | timer < 0 = uncurry translate (playerPosition player) (playerSprite assets)
  | otherwise = Blank
-- and not if we are dead
playerView _ _ = Blank

-- steps the player
playerStep :: Player -> Float -> Player
-- only move if we are alive TODO put this under movable
playerStep p@Player {playerState = Living _, playerHitTimer = timer,playerPowerup = powerUpTimer} dt
  | powerUpTimer < 0 = updatedPlayer {playerWeapon = getDefaultGun DefaultType}
  | otherwise = updatedPlayer
  where
    pv = playerVelocity p
    pp = playerPosition p
    updatedPlayer = p {playerPosition = pp Vector.+ ((dt Prelude.* playerMoveSpeed) Vector.* pv), playerHitTimer = timer - dt, playerPowerup = powerUpTimer - dt}
-- if we are dying, increase the time since death
playerStep p@Player {playerState = Dying t} dt
  | t< 0 = p {playerState = Dead}
  | otherwise = p {playerState = Dying (t Prelude.- dt)}
-- if we are dead, do nothing
playerStep p _ = p


playerClamp :: (Int, Int) -> Player -> Player
playerClamp (screenX,screenY) p@Player {playerPosition = pos} = let
  screenXFloat = int2Float screenX / windowScaling - 8
  screenYFloat = int2Float screenY / windowScaling - 8
  clamp size = (max (negate (size/2)) . min (size/2))
  clampedPos = (clamp screenXFloat (fst pos),clamp screenYFloat (snd pos))
  in p {playerPosition = clampedPos}

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
playerInput p@Player {playerState = Living _} (EventKey (SpecialKey KeySpace) Down _ _) = fireGun p
-- other
playerInput p _ = p

-- implement the gun firing for the player
instance GunUser Player where
  fireGun p@Player {playerWeapon = gun} = p {playerWeapon = setGunFire (1.0, 0.0) gun}
  getGun p@Player {playerWeapon = gun} = gun
  stepGunUser p@Player {playerWeapon = gun} dt = (p {playerWeapon = newGun}, newProjectile)
    where
      (newGun, newProjectile) = stepGun PlayerOwner (playerPosition p Vector.+ (7, 0.5)) gun dt

-- helper function to move
playerAddVelocity :: Player -> Vector -> Player
playerAddVelocity p v = p {playerVelocity = v Vector.+ playerVelocity p}

instance Default Player where
  def = Player (0, 0) (Living 5) (0, 0) (getDefaultGun LaserType) 0 0

instance LivingObject Player where
  isDead (Player _ Dead _ _ _ _) = True
  isDead _ = False
  justDying (Player _ (Dying t) _ _ _ _)
    | t >= 3 = True
    | otherwise = False
  justDying _ = False
