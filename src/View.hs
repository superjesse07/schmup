-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gamestate = do return (playerSprite (assets gamestate))

viewPure :: GameState -> Picture
viewPure gamestate = color white (text "Hello world!")