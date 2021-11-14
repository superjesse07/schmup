-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Assets
import Data.Default
import Data.List
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Gun
import Model
import Player
import System.IO
import System.Random
import State

initialState :: Assets -> GameState
initialState assets = MenuState {assets = assets}

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@PlayingState {player = p, paused = paused}
  | paused = return gstate
  | otherwise = stepps secs gstate
-- if the high scores are empty, save the score and load the high scores from the file system
step secs gs@GameOverState {finalScore = score, highScores = []} = do
  -- add the score to the file
  appendFile "scores.smp" ("\n" ++ show score)
  -- read the file
  content <- readFile' "scores.smp"
  -- parse it
  let scores = map (\x -> read x :: Int) (lines content)
  -- sort it
  let sortedScores = (reverse . sort) scores
  -- put it in the state and return it
  return gs {highScores = sortedScores}
-- otherwise
step secs gstate = return gstate

-- step the playing state
stepps :: Float -> GameState -> IO GameState
stepps dt gs@PlayingState {player = p, bullets = b, turrets = t} = do 
    -- generate new turrets, if any
    newTurrets <- genNewTurrets (3 - length t)
    -- step the projectiles
    let (newPlayer, playerProjectile) = stepGunUser p dt
    -- step the player
    let steppedPlayer = playerStep newPlayer dt 
    -- step the projectiles
    let steppedProjectiles = mapMaybe (stepProjectile dt) (b ++ catMaybes  [playerProjectile])
    -- return the modified gamestate
    return gs {player = steppedPlayer, bullets = steppedProjectiles, turrets = newTurrets ++ t}

genNewTurrets :: Int -> IO [Turret]
genNewTurrets n | n <= 0 = return []
                | otherwise =  do
  -- get the random positions
  x <- randomIO :: IO Float
  y <- randomIO :: IO Float
  -- make other turrets
  rest <- genNewTurrets (n - 1)
  -- make the turret
  let turret = Turret (x * 100.0, y * 100.0) 3
  -- and make all turrets
  return (turret:rest)


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- TODO: use monads for this, as it makes it a lot easier to do with do ... return
inputKey :: Event -> GameState -> GameState
-- on enter pressed, switch to the playing state
inputKey (EventKey (SpecialKey KeyEnter) _ _ _) MenuState {assets = assets} = PlayingState {assets = assets, player = def, paused = False, bullets = [], turrets = []}
-- do the same if we are in the game over screen
-- TODO
-- check for pausing
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gs@PlayingState {paused = p} = gs {paused = not p}
-- end the game, for testing
inputKey (EventKey (SpecialKey KeyEnd) Down _ _) gs@PlayingState {assets = assets} = GameOverState 1 [] assets
-- in the playing state, send inputs to the player
inputKey ip gs@PlayingState {} = gs {player = playerInput (player gs) ip}
-- if we are in game over and enter is pressed, move to the game state again

-- if we are in the playing state, check for arrow keys

--inputKey (EventKey (Char c) _ _ _) gstate
-- = -- If the user presses a character key, show that one
--gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same
