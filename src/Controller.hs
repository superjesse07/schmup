-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Control.Monad.State.Lazy
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

initialState :: StdGen -> GameState 
initialState = MenuState 

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- TODO: use monads for this, as it makes it a lot easier to do with do ... return
inputKey :: Event -> State GameState ()
-- on enter pressed, switch to the playing state
-- inputKey (EventKey (SpecialKey KeyEnter) _ _ _) MenuState { rng = x} = MenuState { rng = x }

-- if we are in game over and enter is pressed, move to the game state again


-- if we are in the playing state, check for arrow keys


--inputKey (EventKey (Char c) _ _ _) gstate
  -- = -- If the user presses a character key, show that one
    --gstate { infoToShow = ShowAChar c }
-- it's possible to update parts
-- gs { pos (x, y + speed)}
--  where (x,y) = pos gs
-- https://wiki.haskell.org/State_Monad
inputKey _ = 
  do
    gs <- get
    let x = rng gs
    return ()