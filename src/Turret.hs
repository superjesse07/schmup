module Turret where

import Graphics.Gloss
import Assets
import System.Random
import Arith
import Debug.Trace
import Consts
import Gun

-- turret enemy
data Turret = Turret
  { turretPosition :: Vector,
    turretHealth :: Int,
    turretWeapon :: Gun,
    turretFireTime :: Float,
    turretTarget :: Vector -- target, this is very large when out of range. Bit ugly, but we do need to remember where we fired at for the gun
  }

-- how to generate new turrets
genNewTurrets :: Int -> IO [Turret]
genNewTurrets n | n <= 0 = return []
                | otherwise =  do
  -- get the random positions
  x <- randomIO :: IO Float
  y <- randomIO :: IO Float
  -- make other turrets
  rest <- genNewTurrets (n - 1)
  -- make the turret
  let turret = Turret (x * 700.0 + 200.0, y * 150.0 - 75.0) 3 (getDefaultGun DefaultType) 0.0 (10000.0, 0.0)
  -- and make all turrets
  return (turret:rest)

-- process a turret
-- this also takes in the player position, so we can use the gun
stepTurret :: Float -> Vector -> Turret -> Maybe Turret 
stepTurret dt ppos t@(Turret position health weapon time target) 
 | vectorTooFar position 800.0 || health <= 0 = Nothing -- remove it when it's out of range
 | otherwise                                  = weaponFireFunc (Just (Turret (position `vectorAdd` (-dt * scrollingSpeed, 0.0)) health weapon (time + dt) ppos))
 where 
   weaponFireFunc | time > 1.0 && vectorDist target position < 150.0 = fireTurretWeapon -- only fire once a second, firing every possible time is too insane and will result in touhou
                  | otherwise  = id
   fireTurretWeapon = fmap (withTimeReset  . fireGun)
   withTimeReset t = t {turretFireTime = 0.0}

-- turrets have a gun, so use that
instance GunUser Turret where 
  -- fire the gun at the player
  fireGun p@Turret {turretWeapon = gun, turretTarget = target, turretPosition = position} = p {turretWeapon = setGunFire (target `vectorSub` position) gun} -- not really used because we have fireWeapon
  getGun = turretWeapon
  stepGunUser p@Turret {turretPosition = position, turretWeapon = gun, turretTarget = target} dt = (p {turretWeapon = newGun}, newProjectile)
    where
      (newGun, newProjectile) = stepGun EnemyOwner (position `vectorAdd` (0,0)) gun dt

-- view a turret
turretView :: Turret -> Assets -> Picture
turretView (Turret v h _ _ _) assets = uncurry translate v (color white (circle 5.0))