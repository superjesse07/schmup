-- | This module defines how to turn
--   the game state into a picture
module View where

import           Assets
import           Graphics.Gloss
import           Model
import           Player

view :: GameState -> IO Picture
view = return . scale 4 4 . viewPure

viewPure :: GameState -> Picture
viewPure MenuState {} = color white (text "Hello world!")
viewPure PlayingState {player = player,assets = assets, paused = paused} = Pictures [playerView player assets, showPaused]
  where 
    showPaused | paused    = color white (text "Paused")
               | otherwise = Blank 
