module Explosion where

import Model
import Graphics.Gloss.Data.Picture (Vector)
import Graphics.Gloss.Data.Point.Arithmetic as Vector
import System.Random (randomIO)

data Explosion = Explosion
    {
     explosionPosition :: Vector,
     explosionAnimation :: Animation,
     explosionsLeft :: Int
    }



stepExplosion :: Float -> Explosion -> IO [Explosion]
stepExplosion dt = 

spawnExplosion :: Int -> Vector -> IO Explosion
spawnExplosion amount position = do
  radius <- randomIO :: IO Float
  angle <- randomIO :: IO Float
  let offset = (cos (angle Prelude.* pi),sin (angle Prelude.* pi))
  return $ Explosion (position Vector.+ ((radius Prelude.* 16) Vector.* offset)) (Animation 0 0) amount