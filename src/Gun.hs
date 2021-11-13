module Gun where 

import Graphics.Gloss
import Assets
import Arith
import Debug.Trace

-- TODO: include whether the weapon is fired (time since firing) and a cooldown
-- then adda  method to fire the weapon and return a maybe projectile
data Gun = LaserGun Float | DefaultGun Float | BurstGun Float Int

-- gun type
data GunType = LaserType | DefaultType | BurstType 

-- get the default gun ready
getDefaultGun :: GunType -> Gun 
getDefaultGun LaserType = LaserGun 1.0
getDefaultGun DefaultType = DefaultGun 1.0
getDefaultGun BurstType = BurstGun 1.0 10

-- set the gun to fire
setGunFire :: Gun -> Gun 
setGunFire (LaserGun t)   | t > 0.6   = LaserGun 0.0
                          | otherwise = LaserGun t 
setGunFire (DefaultGun t) | t > 0.2   = DefaultGun 0.0
                          | otherwise = DefaultGun t
-- only fire if the last volley was shot
setGunFire (BurstGun t n) | t > 0.4 && n >= 6 = BurstGun 0.0 0
                          | otherwise         = BurstGun t n

-- get the projectile
-- step will automatically make it not fire
getGunProjectile :: Vector -> Gun -> Maybe Projectile
getGunProjectile v (LaserGun f)   | f == 0.0          = Just (LaserProjectile v 0.0)
                                  | otherwise         = Nothing
getGunProjectile v (DefaultGun f) | f == 0.0          = Just (DefaultProjectle v)
                                  | otherwise         = Nothing
getGunProjectile v (BurstGun f n) | f == 0.0 && n < 6 = Just (BurstProjectile v)
                                  | otherwise         = Nothing

-- step it so we can increase the time
stepGun :: Vector -> Gun -> Float -> (Gun, Maybe Projectile) 
stepGun v g@(LaserGun f) dt   = (LaserGun (f + dt), getGunProjectile v g)
stepGun v g@(DefaultGun f) dt = (DefaultGun (f + dt), getGunProjectile v g)
-- this one is special. If the timer is high enough, reset it, and increment n
stepGun v g@(BurstGun f n) dt = (newGun, getGunProjectile v g)
  where
    newGun | f > 0.05 && n < 6 = BurstGun 0.0 (n + 1)
           | otherwise         = BurstGun (f + dt) n

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
data Projectile = LaserProjectile Vector Float | DefaultProjectle Vector | BurstProjectile Vector

-- step for them
stepProjectile :: Float -> Projectile -> Maybe Projectile
-- laser projectile, fades after 0.5 seconds
stepProjectile dt (LaserProjectile h t) | t < 0.5   = Just (LaserProjectile h (t + dt))
                                        | otherwise = Nothing
-- default one, dissapears when too far away
stepProjectile dt (DefaultProjectle v) | vectorTooFar v 1000.0 = Nothing 
                                       | otherwise             = Just (DefaultProjectle (v `vectorAdd` (dt * 300.0, 0.0)))
-- same for burst
stepProjectile dt (BurstProjectile v)  | vectorTooFar v 1000.0 = Nothing
                                       | otherwise             = Just (BurstProjectile (v `vectorAdd` (dt * 500.0, 0.0)))

-- view for them
viewProjectile :: Projectile -> Assets -> Picture 
viewProjectile (LaserProjectile h t) _ = uncurry translate (h `vectorAdd` (500.0, 0.0)) (color (withAlpha (1.0 - 2.0 * t) white) (rectangleSolid 1000.0 0.5))
viewProjectile (DefaultProjectle v) _  = uncurry translate v (color white (circleSolid 1.0)) 
viewProjectile (BurstProjectile v) _   = uncurry translate v (color white (rectangleSolid 2.0 0.5)) 