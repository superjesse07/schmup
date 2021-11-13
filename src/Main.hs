module Main where

import Assets
import Controller
import Graphics.Gloss
-- if it doesn't work on linux: run mesa-demo first

import Graphics.Gloss.Interface.IO.Game
import Model
import System.Random
import View

main :: IO ()
main = do
  rng <- newStdGen -- rng
  -- load the sprites
  playerSprite <- loadBMP "assets/ship.bmp" -- sprites

  -- make the assets
  let assets =
        Assets
          { playerSprite = playerSprite
          }

  -- next up, load the actual game
  playIO
    (InWindow "Schmup" (1280, 720) (0, 0)) -- Or FullScreen
    black -- Background color
    60 -- Frames per second
    (initialState rng assets) -- Initial state
    view -- View function
    input -- Event function
    step -- Step function
