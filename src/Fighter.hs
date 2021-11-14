module Fighter where 

import Graphics.Gloss
import Assets
import System.Random
import Arith
import Debug.Trace
import Consts
import Gun
import Avoidance

-- Fighter enemy
data Fighter = Fighter
  { fighterPosition :: Vector,
    fighterHealth :: Int,
    fighterWeapon :: Gun,
    fighterTarget :: Vector -- target, this is very large when out of range. Bit ugly, but we do need to remember where we fired at for the gun
  }

-- how to generate new fighters
genNewFighters :: Int -> IO [Fighter]
genNewFighters n | n <= 0 = return []
                | otherwise =  do
  -- get the random positions
  x <- randomIO :: IO Float
  y <- randomIO :: IO Float
  -- make other fighter
  rest <- genNewFighters (n - 1)
  -- make the fighter
  let fighter = Fighter(x * 700.0 + 200.0, y * 150.0 - 75.0) 3 (getDefaultGun DefaultType) (10000.0, 0.0)
  -- and make all fighters
  return (fighter:rest)

-- process a fighter
-- this also takes in the player position, so we can use the gun
stepFighter :: Float -> Vector -> Fighter -> Maybe Fighter
stepFighter dt ppos t@(Fighter position health weapon target) 
 | vectorTooFar position 800.0 || health <= 0 = Nothing -- remove it when it's out of range
 | otherwise                                  = weaponFireFunc (Just (Fighter moveFunc health weapon ppos))
 where 
   weaponFireFunc | heightDiff < 10.0 && vectorDist target position < 200.0 = fmap fireGun -- only fire once we are on the same y level as the player
                  | otherwise  = id
   heightDiff = abs (snd target - snd position)
   moveFunc = (fst position, snd ppos) `vectorAdd` (-dt * scrollingSpeed, 0.0)

-- fighterss have a gun, so use that
instance GunUser Fighter where 
  -- fire the gun at the player
  fireGun p@Fighter {fighterWeapon = gun, fighterTarget = target, fighterPosition = position} = p {fighterWeapon = setGunFire (-1.0, 0.0) gun} -- not really used because we have fireWeapon
  getGun = fighterWeapon
  stepGunUser p@Fighter {fighterPosition = position, fighterWeapon = gun, fighterTarget = target} dt = (p {fighterWeapon = newGun}, newProjectile)
    where
      (newGun, newProjectile) = stepGun EnemyOwner (position `vectorAdd` (0,0)) gun dt

-- view a fighter
fighterView :: Fighter -> Assets -> Picture
fighterView (Fighter v _ _ _) assets = uncurry translate v (color white (circle 3.0))