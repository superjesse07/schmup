-- | This module defines how to turn
--   the game state into a picture
module View where

import Assets
import Background
import Debug.Trace
import Explosion
import GameState
import Graphics.Gloss
import Gun
import Model
import Player
import Turret
import Graphics.Gloss.Interface.Environment
import GHC.Float.RealFracMethods
import Consts
import Fighter
import Cargo

view :: GameState -> IO Picture
view = return . scale windowScaling windowScaling . viewPure

viewPure :: GameState -> Picture
-- show the main menu
viewPure MenuState {} = color white (text "Hello world!")
-- show the game over screen, score + high scores, ordered low to high
viewPure GameOverState {finalScore = score, highScores = hi} = Pictures ([messageImg, scoreImg] ++ scoreImage)
  where
    messageImg = (scale 0.1 0.1 . color white . text) "Game over"
    scoreImg = (translate 0.0 (-15.0) . scale 0.1 0.1 . color white . text) ("Score: " ++ show score)
    imageScores = map (scale 0.1 0.1 . color white . text . show) hi
    scoreImage = zipWith (\idx img -> translate 0.0 ((- idx) * 15.0 - 30.0) img) [0 .. 10] imageScores

-- show all entities, and the state if paused
viewPure PlayingState {player = player, assets = assets, bullets = bullets, turrets = turrets,fighters = fighters, cargoShips = cargoShips, background = background, explosions = explosions, paused = paused, playingScore = score, screenSize = (x, y)} = Pictures (backgroundPictures ++ (scorePicture : playerPicture : pausedPicture : (projectilePictures ++ turretPictures ++ fighterPictures ++ cargoShipPictures ++ explosionPictures)))
  where
    turretPictures = map (`turretView` assets) turrets
    fighterPictures = map (`fighterView` assets) fighters
    cargoShipPictures = map (`cargoShipView` assets) cargoShips
    playerPicture = playerView player assets
    projectilePictures = map (`viewProjectile` assets) bullets
    explosionPictures = map (viewExplosion assets) explosions
    backgroundPictures = map (viewBackground assets) background
    scorePicture = translate (- int2Float x * 0.125 + 5.0) (- int2Float y * 0.125 + 5.0) (scale 0.1 0.1 (color white (text ("Score: " ++ show score))))
    pausedPicture
      | paused = color white (text "Paused")
      | otherwise = Blank
