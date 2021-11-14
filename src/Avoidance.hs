module Avoidance where 

import Graphics.Gloss

-- move away from the list of points
-- basically, get the closes object, and if there is any close enough, move away from it
avoidAction :: Float -> Float -> [Vector] -> Maybe Vector 
avoidAction dt distance objects = Nothing 
  