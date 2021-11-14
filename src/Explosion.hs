module Explosion where

import Model
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point.Arithmetic as Vector
import System.Random (randomIO)
import Assets

data Explosion = Explosion
    {
     explosionPosition :: Vector,
     explosionAnimation :: Animation,
     explosionsLeft :: Int
    }



stepExplosion :: Float -> Explosion -> IO [Explosion]
stepExplosion dt (Explosion p (Animation f t) l)
  | t > 0.05 && f == 4 = return []
  | t > 0.05 = do
    newExplosion <- spawnExplosion (l Prelude.- 1) p
    let currentExplosion = Explosion p (Animation (f Prelude.+ 1) 0) (l Prelude.- 1)
    if l > 0
    then return [newExplosion,currentExplosion]
    else return [currentExplosion]
  | otherwise = return [Explosion p (Animation f (t Prelude.+ dt)) l]

spawnExplosion :: Int -> Vector -> IO Explosion
spawnExplosion amount position = do
  radius <- randomIO :: IO Float
  angle <- randomIO :: IO Float
  let offset = (cos (angle Prelude.* pi Prelude.* pi),sin (angle Prelude.* pi Prelude.* 2))
  return $ Explosion (position Vector.+ ((radius Prelude.* 8) Vector.* offset)) (Animation 0 0) amount

viewExplosion :: Assets -> Explosion -> Picture
viewExplosion assets (Explosion p (Animation f _) _) = uncurry translate p (explosionSprites assets !! f)