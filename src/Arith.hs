module Arith where

import Graphics.Gloss

-- because yes idk how imports work
vectorAdd :: Vector -> Vector -> Vector
vectorAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vectorSub :: Vector -> Vector -> Vector
vectorSub (ax, ay) (bx, by) = (ax - bx, ay - by)

vectorMulFloat :: Vector -> Float -> Vector
vectorMulFloat (x, y) v = (x * v, y * v)

-- length/norm
vectorLength :: Vector -> Float
vectorLength (x, y) = sqrt (x * x + y * y)

-- distance
vectorDist :: Vector -> Vector -> Float
vectorDist a b = vectorLength (a `vectorSub` b)

-- check if a vector is too far away
vectorTooFar :: Vector -> Float -> Bool
vectorTooFar v f = vectorLength v > f

-- normalize a vector
vectorNormalize :: Vector -> Vector 
vectorNormalize v@(x, y) = (x / vectorLength v, y / vectorLength v) 
