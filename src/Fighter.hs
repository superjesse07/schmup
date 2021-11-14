module Fighter where 

import Graphics.Gloss
import Assets
import System.Random
import Arith
import Debug.Trace
import Consts
import Gun
import Avoidance
import Model

-- Fighter enemy
data Fighter = Fighter
  { fighterPosition :: Vector,
    fighterHealth :: LivingState,
    fighterWeapon :: Gun,
    fighterCourage :: Float, -- how far they dare to go to a player/bullet
    fighterTarget :: Vector, -- target, this is very large when out of range. Bit ugly, but we do need to remember where we fired at for the gun
    fighterHitTimer :: Float
  }

-- how to generate new fighters
genNewFighters :: Int -> IO [Fighter]
genNewFighters n | n <= 0 = return []
                | otherwise =  do
  -- get the random positions
  x <- randomIO :: IO Float
  y <- randomIO :: IO Float
  courage <- randomIO :: IO Float
  -- make other fighter
  rest <- genNewFighters (n - 1)
  -- make the fighter
  let fighter = Fighter(x * 500.0 + 600.0, y * 150.0 - 75.0) (Living 3) (getDefaultGun DefaultType) (courage * 70.0 + 50.0) (10000.0, 0.0) 0
  -- and make all fighters
  return (fighter:rest)

-- process a fighter
-- this also takes in the player position, so we can use the gun
stepFighter :: Float -> [Vector] -> Vector -> Fighter -> Maybe Fighter
stepFighter dt avoid ppos t@(Fighter position (Dying timer) weapon courage target hit) 
 | timer < 0 = Just (Fighter position Dead weapon courage ppos hit) -- remove it when it's dead
 | otherwise = Just (Fighter position (Dying (timer - dt)) weapon courage ppos hit) -- don't move and fire after death
stepFighter dt avoid ppos t@(Fighter position (Living l) weapon courage target hit)
 | vectorTooFar position 800.0 = Nothing -- remove it when it's out of range
 | otherwise                   = weaponFireFunc (Just (Fighter moveFunc (Living l) weapon courage ppos (hit - dt)))
 where 
   weaponFireFunc | abs heightDiff < 10.0 && vectorDist target position < 200.0 = fmap fireGun -- only fire once we are on the same y level as the player
                  | otherwise  = id
   heightDiff = snd target - snd position
   forwardDiff = fst target - fst position
   targetPos = (forwardDiff + courage, heightDiff)
   targetDir = vectorNormalize targetPos 
   targetSpeed = targetDir `vectorMulFloat` (min (dt * vectorLength targetPos) dt * fighterSpeed) -- should avoid jitter, but doesn't 
   moveFunc = case avoidAction dt (courage * 0.25) avoid of 
     Nothing -> position `vectorAdd` (targetDir `vectorMulFloat` (dt * fighterSpeed))
     Just dir -> position `vectorAdd` (dir `vectorMulFloat` (dt * fighterSpeed))

-- fighters have a gun, so use that
instance GunUser Fighter where 
  -- fire the gun at the player
  fireGun p@Fighter {fighterWeapon = gun, fighterTarget = target, fighterPosition = position} = p {fighterWeapon = setGunFire (-1.0, 0.0) gun} 
  getGun = fighterWeapon
  stepGunUser p@Fighter {fighterPosition = position, fighterWeapon = gun, fighterTarget = target} dt = (p {fighterWeapon = newGun}, newProjectile)
    where
      (newGun, newProjectile) = stepGun EnemyOwner (position `vectorAdd` (0,0)) gun dt

-- view a fighter
fighterView :: Fighter -> Assets -> Picture
fighterView (Fighter v _ _ _ _ hit) assets
 | hit < 0 = uncurry translate v (color white (circle 3.0))
 | otherwise = Blank
