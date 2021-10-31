-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gamestate = do picture <- loadBMP "assets/ship.bmp"
                    return$scale 4 4 picture

viewPure :: GameState -> Picture
viewPure gamestate = color white (text "Hello world!")