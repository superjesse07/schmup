module Avoidance where 

import Graphics.Gloss

-- move away from the list of points
avoidAction :: Float -> Float -> [Vector] -> Maybe Vector 
avoidAction dt distance objects = Nothing 
  