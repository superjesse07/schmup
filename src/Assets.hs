module Assets where

import Graphics.Gloss.Data.Picture (Picture)

newtype Assets = Assets
  { playerSprite :: Picture
  }
