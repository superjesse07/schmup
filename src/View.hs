-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player
import Assets

view :: GameState -> IO Picture
view = return . scale 4 4 . viewPure

viewPure :: GameState -> Picture
viewPure MenuState {} = color white (text "Hello world!")
viewPure PlayingState {player = player,assets = assets} = playerView player assets