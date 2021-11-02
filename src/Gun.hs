module Gun where 

-- TODO: include whether the weapon is fired (time since firing) and a cooldown
-- then adda  method to fire the weapon and return a maybe projectile
data Gun = LaserGun Float | DefaultGun Float | BurstGun Float

-- projectile, aka the thing we fired
data Projectile = LaserProjectile | DefaultProjectle | BurstProjectile

-- set the gun to fire
setGunFire :: Gun -> Gun 
setGunFire (LaserGun _)   = LaserGun 0.0
setGunFire (DefaultGun _) = DefaultGun 0.0
setGunFire (BurstGun _)   = BurstGun 0.0

-- get the projectile
-- step will automatically make it not fire
getGunProjectile :: Gun -> Maybe Projectile
getGunProjectile  (LaserGun f)   | f == 0.0  = Just LaserProjectile
                                 | otherwise = Nothing
getGunProjectile  (DefaultGun f) | f == 0.0  = Just DefaultProjectle
                                 | otherwise = Nothing
getGunProjectile  (BurstGun f)   | f == 0.0  = Just BurstProjectile
                                 | otherwise = Nothing

-- step it so we can increase the time
stepGun :: Gun -> Float -> Gun 
stepGun (LaserGun f) dt   = LaserGun (f + dt) 
stepGun (DefaultGun f) dt = DefaultGun (f + dt) 
stepGun (BurstGun f) dt   = BurstGun (f + dt) 

-- class that uses a gun
-- usegun used the gun and gives it a projectile
-- setfiregun fires the gun
class GunUser a where 
  fireGun :: a -> a
  useGun :: a -> Maybe Projectile