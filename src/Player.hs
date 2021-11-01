module Player where

import           Assets
import           Graphics.Gloss

data PlayerState = Alive Int | Dying Float | Dead

data Player = Player
    {
      playerPosition :: Vector,
      playerState    :: PlayerState,
      playerVelocity :: Vector
    }

playerView :: Player -> Assets -> Picture
playerView player assets = uncurry translate (playerPosition player) (playerSprite assets)
