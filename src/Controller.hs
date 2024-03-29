-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Assets
import Background
import Cargo
import Collision
import Consts
import Data.Default
import Data.List
import Data.Maybe
import Debug.Trace
import Explosion
import Fighter
import GameState
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Gun
import Model
import Player
import System.IO
import System.Random
import Turret

initialState :: (Int, Int) -> Assets -> GameState
initialState screenSize assets = MenuState {assets = assets, screenSize = screenSize}

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@PlayingState {player = p, paused = paused}
  | paused = return gstate
  | otherwise = do
    ps <- stepps secs gstate
    return $ explode $ handleCollision ps
-- if the high scores are empty, save the score and load the high scores from the file system
step secs gs@GameOverState {finalScore = score, highScores = []} = do
  -- add the score to the file
  appendFile "scores.smp" (show score ++ "\n")
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
stepps dt gs@PlayingState {player = p, bullets = b, turrets = turrets, explosions = explosions, screenSize = screenSize, background = background, fighters = fighters, cargoShips = cargoShips, cargoDrops = cargoDrops} = do
  -- generate new turrets, if any, hardcode 3 turrets here
  newTurrets <- genNewTurrets (numTurrets - length turrets)
  -- and fighters
  newFighters <- genNewFighters (numFighters - length fighters)
  -- cargo ships
  newCargoShips <- genNewCargoShips (numCargoShips - length cargoShips)
  -- step the projectiles
  let (newPlayer, playerProjectile) = stepGunUser p dt
  let (allTurrets, turretProjectiles) = unzip (map (`stepGunUser` dt) (newTurrets ++ turrets))
  let (allFighters, fighterProjectiles) = unzip (map (`stepGunUser` dt) (newFighters ++ fighters))
  let allCargoShips = newCargoShips ++ cargoShips
  -- step the player
  let steppedPlayer = playerClamp screenSize (playerStep newPlayer dt)
  -- step the projectiles
  let steppedProjectiles = mapMaybe (stepProjectile dt) (b ++ catMaybes [playerProjectile] ++ catMaybes turretProjectiles ++ catMaybes fighterProjectiles)
  -- step the turrets
  let steppedTurrets = mapMaybe (stepTurret dt (playerPosition newPlayer)) allTurrets
  -- things to avoid
  let avoidList = mapMaybe (\x -> if projectileOwner x == PlayerOwner then Just (projectilePosition x) else Nothing) steppedProjectiles ++ [playerPosition newPlayer]
  -- cargo ships
  let steppedCargoShips = mapMaybe (stepCargoShip dt avoidList) allCargoShips
  -- fighters also keep distance from all projectiles + player
  let steppedFighters = mapMaybe (stepFighter dt avoidList (playerPosition newPlayer)) allFighters
  -- step the cargo drops
  let steppedCargoDrops = map (stepCargoPickup dt) cargoDrops
  -- background
  steppedBackground <- backgroundStep screenSize dt background
  mappedExplosions <- mapM (stepExplosion dt) explosions
  let steppedExplosions = concat mappedExplosions

  -- return the modified gamestate
  return gs {player = steppedPlayer, bullets = steppedProjectiles, turrets = steppedTurrets, explosions = steppedExplosions, fighters = steppedFighters, background = steppedBackground, cargoShips = steppedCargoShips, cargoDrops = steppedCargoDrops}

explode :: GameState -> GameState
explode gstate@PlayingState {player = player, turrets = turrets, fighters = fighters, explosions = explosions, cargoDrops = cargoDrops, playingScore = score, cargoShips = cargoShips}
  | isDead player = GameOverState score [] (assets gstate) (screenSize gstate)
  | otherwise = gstate {explosions = explosions ++ newExplosions ++ playerExplosion, cargoDrops = cargoDrops ++ newCargoDrops, playingScore = score + (length newExplosions * 100)} -- add 100 points for every dead enemy
  where
    turretExplosions = map (newExplosion 4 . turretPosition) (filter justDying turrets) -- spawns explosions for dead turrets
    fighterExplosions = map (newExplosion 4 . fighterPosition) (filter justDying fighters) -- spawns explosions for dead fighters
    cargoExplosions = map (newExplosion 4 . cargoShipPosition) (filter justDying cargoShips)-- spawns explosions for dead cargoships
    newCargoDrops = mapMaybe fromCargoShip (filter justDying cargoShips) -- spawns new cargo drops
    playerExplosion -- Creates a explosion for the player if they died
      | justDying player = [newExplosion 10 (playerPosition player)]
      | otherwise = []
    newExplosions = turretExplosions ++ fighterExplosions ++ cargoExplosions

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

getPlayState :: (Int, Int) -> Assets -> GameState
getPlayState screenSize assets =
  PlayingState
    { assets = assets,
      player = def,
      paused = False,
      bullets = [],
      turrets = [],
      fighters = [],
      explosions = [],
      playingScore = 0,
      screenSize = screenSize,
      background = [],
      cargoDrops = [],
      cargoShips = []
    }

inputKey :: Event -> GameState -> GameState
-- resize the screen
inputKey (EventResize (x, y)) gs = gs {screenSize = (x, y)}
-- on enter pressed, switch to the playing state
inputKey (EventKey (SpecialKey KeyEnter) _ _ _) MenuState {assets = assets, screenSize = screenSize} = getPlayState screenSize assets
-- check for pausing
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gs@PlayingState {paused = p} = gs {paused = not p}
-- end the game, for testing
inputKey (EventKey (SpecialKey KeyEnd) Down _ _) gs@PlayingState {assets = assets, playingScore = score, screenSize = screenSize} = GameOverState score [] assets screenSize
-- in the playing state, send inputs to the player
inputKey ip gs@PlayingState {} = gs {player = playerInput (player gs) ip}
-- if we are in game over and enter is pressed, move to the game state again
inputKey (EventKey (SpecialKey KeyEnter) _ _ _) GameOverState {assets = assets, screenSize = screenSize} = getPlayState screenSize assets
-- Otherwise keep the same
inputKey _ gstate = gstate
