module Cargo where

import Graphics.Gloss
import Assets
import System.Random
import Arith
import Debug.Trace
import Consts
import Gun
import Model
import Avoidance


-- cargo ship pickup
data CargoPickup = CargoPickup {
    cargoPosition :: Vector,
    cargoType :: GunType
}

-- generate a cargo pickup from the ship
fromCargoShip :: CargoShip -> Maybe CargoPickup
fromCargoShip CargoShip { cargoShipPosition = pos, cargoShipPickup = Just cargoType} = Just (CargoPickup pos cargoType)
fromCargoShip _ = Nothing

-- step it
stepCargoPickup :: Float -> CargoPickup -> CargoPickup
stepCargoPickup dt (CargoPickup pos cargoType) = CargoPickup (pos `vectorAdd` (-dt * scrollingSpeed, 0.0)) cargoType

-- view it
cargoPickupView :: CargoPickup -> Assets -> Picture 
cargoPickupView (CargoPickup v _) assets = uncurry translate v (cargoSprite assets)

-- cargoShip enemy
data CargoShip = CargoShip
  { cargoShipPosition :: Vector,
    cargoShipHealth :: LivingState,
    cargoShipPickup :: Maybe GunType,
    cargoShipHitTimer :: Float
  }

-- how to generate new cargoShips
genNewCargoShips :: Int -> IO [CargoShip]
genNewCargoShips n | n <= 0 = return []
                   | otherwise =  do
  -- get the random positions
  x <- randomIO :: IO Float
  y <- randomIO :: IO Float
  i <- randomIO :: IO Int
  -- make the wrapped int
  let wrappedInt = i `mod` 4
  -- make other cargoShips
  rest <- genNewCargoShips (n - 1)
  -- make the cargoShip
  let cargoShip = CargoShip (x * 700.0 + 200.0, y * 150.0 - 75.0) (Living 3) (typeFromInt wrappedInt) 0.0
  -- and make all cargoShips
  return (cargoShip:rest)

-- process a cargoShip
-- this also takes in what we want to avoid
stepCargoShip :: Float -> [Vector] -> CargoShip -> Maybe CargoShip
stepCargoShip dt avoid t@(CargoShip position (Dying timer) pickup hit)
  | timer < 0 = Nothing
  | otherwise = Just (CargoShip position (Dying (timer - dt)) pickup 0)
stepCargoShip dt avoid t@(CargoShip position (Living health) pickup hit)
 | vectorTooFar position 800.0 = Nothing -- remove it when it's out of range
 | otherwise                   = Just (CargoShip (moveFunc `vectorAdd` (-dt * scrollingSpeed, 0.0)) (Living health) pickup (hit-dt))
 where
  moveFunc = case avoidAction 50.0 position avoid of
     Nothing  -> position `vectorAdd` (-dt * cargoSpeed, if snd position > 0.0 then -dt * cargoSpeed * 0.5 else dt * cargoSpeed * 0.5)
     Just dir -> position `vectorAdd` (dir `vectorMulFloat` (dt * cargoSpeed))

-- view a cargoShip
cargoShipView :: CargoShip -> Assets -> Picture
cargoShipView (CargoShip v h pickup hit) assets
 | hit < 0 = uncurry translate v (color red (cargoShipSprite assets))
 | otherwise = Blank

instance LivingObject CargoShip where
  isDead (CargoShip _ Dead _ _) = True
  isDead _ = False
  justDying (CargoShip _ (Dying t) _ _)
    | t >= 1 = True
    | otherwise = False
  justDying _ = False