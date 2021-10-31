module Main where

import Controller
import Model
import View
import System.Random

-- if it doesn't work on linux: run mesa-demo first

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
       rng <- newStdGen 
       playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black              -- Background color
              30                 -- Frames per second
              (initialState rng) -- Initial state
              view               -- View function
              input              -- Event function
              step               -- Step function