module Turret where

import Graphics.Gloss
import Assets
import System.Random
import Arith
import Debug.Trace
import Consts

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
  let turret = Turret (x * 700.0 + 200.0, y * 150.0 - 75.0) 3
  -- and make all turrets
  return (turret:rest)

-- process a turret
-- this also takes in the player position, so we can use the gun
stepTurret :: Float -> Vector -> Turret -> Maybe Turret 
stepTurret dt ppos t@(Turret p h) | vectorTooFar p 800.0 = Nothing 
                                  | otherwise            = Just (Turret (p `vectorAdd` (-dt * scrollingSpeed, 0.0)) h)

-- turrets have a gun, so use that

-- view a turret
turretView :: Turret -> Assets -> Picture
turretView (Turret v h) assets = uncurry translate v (color white (circle 5.0))