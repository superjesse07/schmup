module Background where

import Graphics.Gloss.Data.Picture (Vector)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random.Stateful (randomIO)
import GHC.Float (int2Float)

data BackgroundObject = Star Vector Float | Asteroid Vector Float

backgroundStep :: Float -> [BackgroundObject] -> IO [BackgroundObject]
backgroundStep dt background
  | length background < 100 = do
    newObject <- spawnBackgroundObject
    return $ newObject : map (backgroundObjectStep dt) background
  | otherwise = return $ map (backgroundObjectStep dt) background

backgroundObjectStep :: Float -> BackgroundObject -> BackgroundObject
backgroundObjectStep = undefined

spawnBackgroundObject :: IO BackgroundObject
spawnBackgroundObject = do
  screenSize <- getScreenSize
  randomHeight <- randomIO :: IO Float
  speed <- randomIO :: IO Float
  let position = (int2Float (fst screenSize) + 100,randomHeight * int2Float (snd screenSize))
  return $ Star position speed
