module Turret where

import Graphics.Gloss
import Assets
import System.Random

-- turret enemy
data Turret = Turret
  { turretPosition :: Vector,
    turretHealth :: Int
  }

-- how to generate new turrets
genNewTurrets :: Int -> IO [Turret]
genNewTurrets n | n <= 0 = return []
                | otherwise =  do
  -- get the random positions
  x <- randomIO :: IO Float
  y <- randomIO :: IO Float
  -- make other turrets
  rest <- genNewTurrets (n - 1)
  -- make the turret
  let turret = Turret (x * 100.0, y * 100.0) 3
  -- and make all turrets
  return (turret:rest)

-- process a turret

-- turrets have a gun, so use that

-- view a turret
turretView :: Turret -> Assets -> Picture
turretView (Turret v h) assets = uncurry translate v (color white (circle 5.0))