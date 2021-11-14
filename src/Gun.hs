module Gun where

import Arith
import Assets
import Debug.Trace
import Graphics.Gloss

-- ownership, wether the enemy or player owns it
data OwnerShip = PlayerOwner | EnemyOwner

-- TODO: include whether the weapon is fired (time since firing) and a cooldown
-- then adda  method to fire the weapon and return a maybe projectile
data Gun = LaserGun Float | DefaultGun Vector Float | BurstGun Vector Float Int

-- gun type
data GunType = LaserType | DefaultType | BurstType

-- get the default gun ready
getDefaultGun :: GunType -> Gun
getDefaultGun LaserType = LaserGun 1.0
getDefaultGun DefaultType = DefaultGun (1.0, 0.0) 1.0
getDefaultGun BurstType = BurstGun (1.0, 0.0) 1.0 10

-- set the gun to fire
setGunFire :: Vector -> Gun -> Gun
setGunFire d (LaserGun t)
  | t > 0.4 = LaserGun 0.0
  | otherwise = LaserGun t
setGunFire d (DefaultGun v t)
  | t > 0.2 = DefaultGun d 0.0
  | otherwise = DefaultGun v t
-- only fire if the last volley was shot
setGunFire d (BurstGun v t n)
  | t > 0.4 && n >= 6 = BurstGun d 0.0 0
  | otherwise = BurstGun v t n

-- get the projectile
-- step will automatically make it not fire
getGunProjectile :: OwnerShip -> Vector -> Gun -> Maybe Projectile
getGunProjectile o v (LaserGun f)
  | f == 0.0 = Just (LaserProjectile o v 0.0)
  | otherwise = Nothing
getGunProjectile o v (DefaultGun d f)
  | f == 0.0 = Just (DefaultProjectile o (vectorNormalize d) v)
  | otherwise = Nothing
getGunProjectile o v (BurstGun d f n)
  | f == 0.0 && n < 6 = Just (BurstProjectile o (vectorNormalize d) v)
  | otherwise = Nothing

-- step it so we can increase the time
stepGun :: OwnerShip -> Vector -> Gun -> Float -> (Gun, Maybe Projectile)
stepGun o v g@(LaserGun f) dt = (LaserGun (f + dt), getGunProjectile o v g)
stepGun o v g@(DefaultGun d f) dt = (DefaultGun d (f + dt), getGunProjectile o v g)
-- this one is special. If the timer is high enough, reset it, and increment n
stepGun o v g@(BurstGun d f n) dt = (newGun, getGunProjectile o v g)
  where
    newGun
      | f > 0.05 && n < 6 = BurstGun d 0.0 (n + 1)
      | otherwise = BurstGun d (f + dt) n

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
data Projectile = LaserProjectile OwnerShip Vector Float | DefaultProjectile OwnerShip Vector Vector | BurstProjectile OwnerShip Vector Vector

-- position of the projectile
projectilePosition :: Projectile -> Vector 
projectilePosition (LaserProjectile _ p _) = p
projectilePosition (DefaultProjectile _ _ p) = p
projectilePosition (BurstProjectile _ _ p) = p


isPlayerBullet :: Projectile -> Bool
isPlayerBullet (LaserProjectile PlayerOwner _ _) = True
isPlayerBullet (DefaultProjectile PlayerOwner _ _) = True
isPlayerBullet (BurstProjectile PlayerOwner _ _) = True
isPlayerBullet _ = False

-- step for them
stepProjectile :: Float -> Projectile -> Maybe Projectile
-- laser projectile, fades after 0.5 seconds
stepProjectile dt (LaserProjectile o h t)
  | t < 0.2 = Just (LaserProjectile o h (t + dt))
  | otherwise = Nothing
-- default one, disappears when too far away
stepProjectile dt (DefaultProjectile o d v)
  | vectorTooFar v 1000.0 = Nothing
  | otherwise = Just (DefaultProjectile o d (v `vectorAdd` (d `vectorMulFloat` (dt * 300.0))))
-- same for burst
stepProjectile dt (BurstProjectile o d v)
  | vectorTooFar v 1000.0 = Nothing
  | otherwise = Just (BurstProjectile o d (v `vectorAdd` (d `vectorMulFloat` (dt * 200.0))))

-- view for them 
viewProjectile :: Projectile -> Assets -> Picture
viewProjectile (LaserProjectile _ h t) assets@Assets {laserSprite = ls} = uncurry translate (h `vectorAdd` (500.0, 0.0)) (scale 1000.0 (1.0 - (a * a * a)) ls)
  where a = 4.0 * t
viewProjectile (DefaultProjectile _ _ v) assets@Assets {bulletSprite = bs} = uncurry translate v bs
viewProjectile (BurstProjectile _ _ v) assets@Assets {bulletSprite = bs} = uncurry translate v bs
