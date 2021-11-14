module Assets where

import Graphics.Gloss.Data.Picture (Picture)

data Assets = Assets
  { playerSprite :: Picture,
    laserSprite :: Picture,
    cargoShipSprite :: Picture,
    bulletSprite :: Picture,
    explosionSprites :: [Picture],
    starSprite :: Picture
  }
