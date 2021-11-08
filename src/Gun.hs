module Gun where 

import Graphics.Gloss
import Assets
import Arith
import Debug.Trace

-- TODO: include whether the weapon is fired (time since firing) and a cooldown
-- then adda  method to fire the weapon and return a maybe projectile
data Gun = LaserGun Float | DefaultGun Float | BurstGun Float

-- set the gun to fire
setGunFire :: Gun -> Gun 
setGunFire (LaserGun t)   | t > 0.5   = LaserGun 0.0
                          | otherwise = LaserGun t 
setGunFire (DefaultGun t) | t > 0.2   = DefaultGun 0.0
                          | otherwise = DefaultGun t
setGunFire (BurstGun t)   | t > 0.4   = BurstGun 0.0
                          | otherwise = BurstGun t

-- get the projectile
-- step will automatically make it not fire
getGunProjectile :: Vector -> Gun -> Maybe Projectile
getGunProjectile v (LaserGun f)   | f == 0.0  = Just LaserProjectile
                                  | otherwise = Nothing
getGunProjectile v (DefaultGun f) | f == 0.0  = Just (DefaultProjectle v)
                                  | otherwise = Nothing
getGunProjectile v (BurstGun f)   | f == 0.0  = Just BurstProjectile
                                  | otherwise = Nothing

-- step it so we can increase the time
stepGun :: Vector -> Gun -> Float -> (Gun, Maybe Projectile) 
stepGun v g@(LaserGun f) dt   = (LaserGun (f + dt), getGunProjectile v g)
stepGun v g@(DefaultGun f) dt = (DefaultGun (f + dt), getGunProjectile v g)
stepGun v g@(BurstGun f) dt   = (BurstGun (f + dt), getGunProjectile v g)

-- class that uses a gun
-- usegun used the gun and gives it a projectile
-- setfiregun fires the gun
class GunUser a where
  -- fire the weapon 
  fireGun :: a -> a
  -- get it from the entity
  getGun :: a -> Gun
  -- use it, as in get the projectile
  -- provide a default impl here
  -- this works by doing the gun step seperate from the entity step. This steps the entity's gun
  stepGunUser :: a -> Float -> (a, Maybe Projectile)

-- projectile logic
-- projectile, aka the thing we fired
data Projectile = LaserProjectile | DefaultProjectle Vector | BurstProjectile

-- step for them
stepProjectile :: Float -> Projectile -> Projectile
stepProjectile dt LaserProjectile       = LaserProjectile
stepProjectile dt (DefaultProjectle v)  = DefaultProjectle ((traceShowId v) `vectorAdd` (dt * 100.0, 0.0))
stepProjectile dt BurstProjectile       = BurstProjectile

-- view for them
viewProjectile :: Projectile -> Assets -> Picture 
viewProjectile LaserProjectile _      = Blank 
viewProjectile (DefaultProjectle v) _ = uncurry translate v (color white (circleSolid 1.0)) 
viewProjectile BurstProjectile _      = Blank