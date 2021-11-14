module Explosion where

import Assets
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point.Arithmetic as Vector
import Model
import System.Random (randomIO)

data Explosion = Explosion
  { explosionPosition :: Vector,
    explosionAnimation :: Animation,
    explosionsLeft :: Int
  }

stepExplosion :: Float -> Explosion -> IO [Explosion]
stepExplosion dt (Explosion p (Animation f t) l)
  | t > 0.05 && f == 4 = return [] -- If it's on the last frame return an empty list
  | t > 0.05 && l > 0 = do -- go to next frame spawn a new explosion
    newExplosion <- spawnExplosion (l Prelude.- 1) p
    let currentExplosion = Explosion p (Animation (f Prelude.+ 1) 0) (l Prelude.- 1)
    return [newExplosion, currentExplosion]
  | t > 0.05 = return [Explosion p (Animation (f Prelude.+ 1) 0) 0] -- just go to next frame
  | otherwise = return [Explosion p (Animation f (t Prelude.+ dt)) l] -- just add to the timer

newExplosion :: Int -> Vector -> Explosion -- creates a new explosion at the specified point with the specified amount
newExplosion amount position = Explosion position (Animation 0 0) amount

spawnExplosion :: Int -> Vector -> IO Explosion -- Spawns an explosion at an offset of the previous one
spawnExplosion amount position = do
  radius <- randomIO :: IO Float
  angle <- randomIO :: IO Float
  randomT <- randomIO :: IO Float
  let randomTime = randomT Prelude.* 0.1
  let offset = (cos (angle Prelude.* pi Prelude.* pi), sin (angle Prelude.* pi Prelude.* 2))
  return $ Explosion (position Vector.+ ((radius Prelude.* 8) Vector.* offset)) (Animation 0 (Prelude.negate randomTime)) amount

viewExplosion :: Assets -> Explosion -> Picture
viewExplosion assets (Explosion p (Animation f _) _) = uncurry translate p (explosionSprites assets !! f)
