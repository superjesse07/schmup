module Player where

import Assets
import Graphics.Gloss

import Graphics.Gloss.Interface.IO.Game
import Data.Default
import Arith

data PlayerState = Alive Int | Dying Float | Dead

data Player = Player
    {
      playerPosition :: Vector,
      playerState    :: PlayerState,
      playerVelocity :: Vector
    }

-- shows the player
playerView :: Player -> Assets -> Picture
playerView player assets = uncurry translate (playerPosition player) (playerSprite assets)

-- steps the player
playerStep :: Player -> Float -> Player 
playerStep p dt = p { playerPosition = playerVelocity p `vectorAdd` playerPosition p }

-- bit verbose but hey ho
playerInput :: Player -> Event -> Player
-- up
playerInput p (EventKey (SpecialKey KeyUp) Up _ _) = playerAddVelocity p (0.0, -10.0)
playerInput p (EventKey (SpecialKey KeyUp) Down _ _) = playerAddVelocity p (0.0, 10.0)
-- down
playerInput p (EventKey (SpecialKey KeyDown) Up _ _) = playerAddVelocity p (0.0, 10.0)
playerInput p (EventKey (SpecialKey KeyDown) Down _ _) = playerAddVelocity p (0.0, -10.0)
-- left
playerInput p (EventKey (SpecialKey KeyLeft) Up _ _) = playerAddVelocity p (10.0, 0.0)
playerInput p (EventKey (SpecialKey KeyLeft) Down _ _) = playerAddVelocity p (-10.0, 0.0)
-- right
playerInput p (EventKey (SpecialKey KeyRight) Up _ _) = playerAddVelocity p (-10.0, 0.0)
playerInput p (EventKey (SpecialKey KeyRight) Down _ _) = playerAddVelocity p (10.0, 0.0)
-- other
playerInput p _ = p

-- helper function to move
playerAddVelocity :: Player -> Vector -> Player
playerAddVelocity p v = p { playerVelocity = v `vectorAdd` playerVelocity p }

instance Default Player where
  def = Player (0,0) (Alive 5) (0,0)