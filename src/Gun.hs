module Gun where 

-- TODO: include whether the weapon is fired (time since firing) and a cooldown
-- then adda  method to fire the weapon and return a maybe projectile
data Gun = LaserGun Float | DefaultGun Float | BurstGun Float

-- projectile, aka the thing we fired
data Projectile = LaserProjectile | DefaultProjectle | BurstProjectile

-- and fire it, TODO vector for position
fireGun :: Gun -> Maybe Projectile
fireGun (LaserGun f)   | f < 0.5   = Just LaserProjectile
                       | otherwise = Nothing
fireGun (DefaultGun f) | f < 0.5   = Just DefaultProjectle
                       | otherwise = Nothing
fireGun (BurstGun f)   | f < 0.5   = Just BurstProjectile
                       | otherwise = Nothing

-- step it so we can increase the time
stepGun :: Gun -> Float -> Gun 
stepGun (LaserGun f) dt   = LaserGun (f + dt) 
stepGun (DefaultGun f) dt = DefaultGun (f + dt) 
stepGun (BurstGun f) dt   = BurstGun (f + dt) 

-- class that uses a gun
class GunUser a where 
  useGun :: a -> Maybe Projectile