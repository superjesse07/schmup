-- | This module defines how to turn
--   the game state into a picture
module View where

import Assets
import Debug.Trace
import Graphics.Gloss
import Gun
import Model
import Player
import GameState

view :: GameState -> IO Picture
view = return . scale 4 4 . viewPure

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
viewPure PlayingState {player = player, assets = assets, bullets = bullets, paused = paused} = Pictures (playerPicture : pausedPicture : projectilePictures)
  where
    playerPicture = playerView player assets
    projectilePictures = map (`viewProjectile` assets) bullets
    pausedPicture
      | paused = color white (text "Paused")
      | otherwise = Blank
