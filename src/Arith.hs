module Arith where

import Graphics.Gloss

-- because yes idk how imports work
vectorAdd :: Vector -> Vector -> Vector
vectorAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vectorMulFloat :: Vector -> Float -> Vector 
vectorMulFloat (x, y) v = (x * v, y * v)