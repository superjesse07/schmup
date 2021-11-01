-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

initialState :: StdGen -> Assets -> GameState 
initialState gen assets = MenuState { assets = assets, rng = gen }

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- TODO: use monads for this, as it makes it a lot easier to do with do ... return
inputKey :: Event -> GameState -> GameState
-- on enter pressed, switch to the playing state
inputKey (EventKey (SpecialKey KeyEnter) _ _ _) MenuState { rng = x} = MenuState { rng = x }

-- if we are in game over and enter is pressed, move to the game state again


-- if we are in the playing state, check for arrow keys


--inputKey (EventKey (Char c) _ _ _) gstate
  -- = -- If the user presses a character key, show that one
    --gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same