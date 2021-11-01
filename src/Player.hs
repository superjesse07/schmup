module Player where

import Assets
import Graphics.Gloss
import Data.Default

data PlayerState = Alive Int | Dying Float | Dead

data Player = Player
    {
      playerPosition :: Vector,
      playerState    :: PlayerState,
      playerVelocity :: Vector
    }

playerView :: Player -> Assets -> Picture
playerView player assets = uncurry translate (playerPosition player) (playerSprite assets)

instance Default Player where
  def = Player (0,0) (Alive 5) (0,0)