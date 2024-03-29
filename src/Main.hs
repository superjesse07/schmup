module Main where

import Assets
import Controller
-- if it doesn't work on linux: run mesa-demo first

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Model
import System.Random
import View

main :: IO ()
main = do
  -- load the sprites
  playerSprite <- loadTexture "assets/ship.png"
  laserSprite <- loadTexture "assets/laser.png"
  explosionSprites <- loadAnimation "assets/explosion" 5
  cargoShipSprite <- loadTexture "assets/cargoship.png"
  starSprite <- loadTexture "assets/star.png"
  turretSprite <- loadTexture "assets/turret.png"
  bulletSprite <- loadTexture "assets/bullet.png"
  fighterSprite <- loadTexture "assets/fighter.png"
  turretSprite <- loadTexture "assets/turret.png"
  cargoSprite <- loadTexture "assets/cargo.png"

  -- make the assets
  let assets =
        Assets
          { playerSprite = playerSprite,
            laserSprite = laserSprite,
            explosionSprites = explosionSprites,
            bulletSprite = bulletSprite,
            cargoShipSprite = cargoShipSprite,
            starSprite = starSprite,
            fighterSprite = fighterSprite,
            turretSprite = turretSprite,
            cargoSprite = cargoSprite
          }

  -- next up, load the actual game
  playIO
    (InWindow "Schmup" (1280, 720) (0, 0)) -- Or FullScreen
    black -- Background color
    60 -- Frames per second
    (initialState (1280, 720) assets) -- Initial state
    view -- View function
    input -- Event function
    step -- Step function

loadTexture :: FilePath -> IO Picture
loadTexture path = do
  texture <- loadJuicy path
  missing <- loadBMP "assets/missing.bmp"
  return $ fromMaybe missing texture

loadAnimation :: FilePath -> Int -> IO [Picture]
loadAnimation path frame = do
  let framepath = path ++ show frame ++ ".png"
  texture <- loadTexture framepath
  if frame == 1
    then return [texture]
    else do
      textures <- loadAnimation path (frame - 1)
      return $ textures ++ [texture]
