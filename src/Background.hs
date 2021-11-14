module Background where

import Assets
import Consts
import Data.Maybe (mapMaybe)
import GHC.Float (int2Float)
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vector
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random.Stateful (randomIO)
import Debug.Trace (traceIO)

data BackgroundObject = Star Vector Float | Asteroid Vector Float

backgroundStep :: (Int, Int) -> Float -> [BackgroundObject] -> IO [BackgroundObject]
backgroundStep screenSize dt background = do
    newObject <- spawnBackgroundObject screenSize
    return $ newObject : mapMaybe (backgroundObjectStep screenSize dt) background

backgroundObjectStep :: (Int, Int) -> Float -> BackgroundObject -> Maybe BackgroundObject
backgroundObjectStep screenSize dt (Star pos speed)
  | fst pos > (- int2Float (fst screenSize)) = Just (Star (pos Vector.- (speed * dt, 0)) speed)
  | otherwise = Nothing
backgroundObjectStep screenSize dt (Asteroid pos speed)
  | fst pos > (- int2Float (fst screenSize)) = Just (Asteroid (pos Vector.- (speed * dt, 0)) speed)
  | otherwise = Nothing

spawnBackgroundObject :: (Int, Int) -> IO BackgroundObject
spawnBackgroundObject screenSize = do
  randomHeight <- randomIO :: IO Float
  speed <- randomIO :: IO Float

  let position = (int2Float (fst screenSize), (randomHeight * 2 - 1) * int2Float (snd screenSize))
  return $ Star (fst position / windowScaling, snd position / windowScaling) ((speed + 1) * 100)

viewBackground :: Assets -> BackgroundObject -> Picture
viewBackground assets (Star pos _) = uncurry translate pos (starSprite assets)
