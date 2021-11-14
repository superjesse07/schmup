module Avoidance where

import Arith
import Graphics.Gloss

-- move away from the list of points
-- basically, get the closes object, and if there is any close enough, move away from it
avoidAction :: Float -> Vector -> [Vector] -> Maybe Vector
avoidAction distance position objects
  | dist < distance = Just (vectorNormalize (position `vectorSub` obj))
  | otherwise = Nothing
  where
    (dist, obj) = foldl closestPoint (distance + 1.0, (0.0, 0.0)) objects -- distance + 1 is guaranteed to return Maybe
    closestPoint (curDist, curPos) newPos
      | vectorDist newPos position < curDist = (vectorDist newPos position, newPos)
      | otherwise = (curDist, curPos)
