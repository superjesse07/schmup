module Gun where

import Arith
import Assets
import Debug.Trace
import Graphics.Gloss

-- ownership, wether the enemy or player owns it
data OwnerShip = PlayerOwner | EnemyOwner

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
setGunFire (LaserGun t)
  | t > 0.4 = LaserGun 0.0
  | otherwise = LaserGun t
setGunFire (DefaultGun t)
  | t > 0.2 = DefaultGun 0.0
  | otherwise = DefaultGun t
-- only fire if the last volley was shot
setGunFire (BurstGun t n)
  | t > 0.4 && n >= 6 = BurstGun 0.0 0
  | otherwise = BurstGun t n

-- get the projectile
-- step will automatically make it not fire
getGunProjectile :: OwnerShip -> Vector -> Gun -> Maybe Projectile
getGunProjectile o v (LaserGun f)
  | f == 0.0 = Just (LaserProjectile o v 0.0)
  | otherwise = Nothing
getGunProjectile o v (DefaultGun f)
  | f == 0.0 = Just (DefaultProjectile o v)
  | otherwise = Nothing
getGunProjectile o v (BurstGun f n)
  | f == 0.0 && n < 6 = Just (BurstProjectile o v)
  | otherwise = Nothing

-- step it so we can increase the time
stepGun :: OwnerShip -> Vector -> Gun -> Float -> (Gun, Maybe Projectile)
stepGun o v g@(LaserGun f) dt = (LaserGun (f + dt), getGunProjectile o v g)
stepGun o v g@(DefaultGun f) dt = (DefaultGun (f + dt), getGunProjectile o v g)
-- this one is special. If the timer is high enough, reset it, and increment n
stepGun o v g@(BurstGun f n) dt = (newGun, getGunProjectile o v g)
  where
    newGun
      | f > 0.05 && n < 6 = BurstGun 0.0 (n + 1)
      | otherwise = BurstGun (f + dt) n

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
data Projectile = LaserProjectile OwnerShip Vector Float | DefaultProjectile OwnerShip Vector | BurstProjectile OwnerShip Vector

-- step for them
stepProjectile :: Float -> Projectile -> Maybe Projectile
-- laser projectile, fades after 0.5 seconds
stepProjectile dt (LaserProjectile o h t)
  | t < 0.2 = Just (LaserProjectile o h (t + dt))
  | otherwise = Nothing
-- default one, disappears when too far away
stepProjectile dt (DefaultProjectile o v)
  | vectorTooFar v 1000.0 = Nothing
  | otherwise = Just (DefaultProjectile o (v `vectorAdd` (dt * 300.0, 0.0)))
-- same for burst
stepProjectile dt (BurstProjectile o v)
  | vectorTooFar v 1000.0 = Nothing
  | otherwise = Just (BurstProjectile o (v `vectorAdd` (dt * 200.0, 0.0)))

-- view for them 
viewProjectile :: Projectile -> Assets -> Picture
viewProjectile (LaserProjectile _ h t) assets@Assets {laserSprite = ls} = uncurry translate (h `vectorAdd` (500.0, 0.0)) (scale 1000 1 ls)
viewProjectile (DefaultProjectile _ v) _ = uncurry translate v (color white (circleSolid 1.0))
viewProjectile (BurstProjectile _ v) _ = uncurry translate v (color white (rectangleSolid 2.0 0.5))
