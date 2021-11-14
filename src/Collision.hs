module Collision where

import Graphics.Gloss.Data.Picture


class Collision a where
  getHitBox :: a -> (Vector,Vector)