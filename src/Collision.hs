module Collision where

import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vector
import Gun
import Model
import Player
import Turret
import Fighter
import GameState
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace (trace)

isColliding :: (Vector, Vector) -> (Vector, Vector) -> Bool
isColliding ((leftA, bottomA), (rightA, topA)) ((leftB, bottomB), (rightB, topB)) = leftA < rightB && rightA > leftB && bottomA < topB && topA > bottomB

handleCollision :: GameState -> GameState
handleCollision gstate@PlayingState {player = player, turrets = turrets, fighters = fighters, bullets = bullets} = let

  collidedPlayer = collideWith (player) $ filter (not . isPlayerBullet) bullets
  collidedFighters = mapMaybe (`collideWith` filter isPlayerBullet bullets) fighters
  collidedTurret = mapMaybe (`collideWith` filter isPlayerBullet bullets) turrets
  collidedBulletsEnemy = mapMaybe (`collideWith` [player]) $ filter (not . isPlayerBullet) bullets
  collidedBulletsPlayer = mapMaybe ((`collideWithMaybe` fighters) . (`collideWith` turrets)) $ filter isPlayerBullet bullets

  in gstate {player = fromMaybe player collidedPlayer, bullets = collidedBulletsEnemy ++ collidedBulletsPlayer,fighters=collidedFighters,turrets = collidedTurret}




collideWith :: Collision a => Collision b => a -> [b] -> Maybe a
collideWith a = collideWithMaybe (Just a)


collideWithMaybe :: Collision a => Collision b => Maybe a -> [b] -> Maybe a
collideWithMaybe Nothing _ = Nothing
collideWithMaybe (Just collider) [] = Just collider
collideWithMaybe (Just collider) (x:xs) = collideWithMaybe (checkCollision collider x) xs

class Collision a where
  getHitBox :: a -> (Vector, Vector)
  checkCollision :: Collision b => a -> b -> Maybe a

instance Collision Player where
  getHitBox p@Player {playerPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision p@Player {playerState = (Living health), playerHitTimer = timer} other
    | timer > 0 = Just p
    | isColliding (getHitBox p) (getHitBox other) && health == 1 = Just p {playerState = Dying 1}
    | isColliding (getHitBox p) (getHitBox other) = Just p {playerState = Living (health - 1), playerHitTimer = 0.2}
  checkCollision p other = Just p

instance Collision Projectile where
  getHitBox (DefaultProjectile _ _ pos) = (pos Vector.- (1, 1), pos Vector.+ (1, 1))
  getHitBox (BurstProjectile _ _ pos) = (pos Vector.- (1, 1), pos Vector.+ (1, 1))
  getHitBox (LaserProjectile _ pos _) = (pos Vector.- (1, 1), pos Vector.+ (1, 1))
  checkCollision this other
    | isColliding (getHitBox this) (getHitBox other) = Nothing
    | otherwise = Just this

instance Collision Turret where
  getHitBox t@Turret {turretPosition = pos} = (pos Vector.- (8, 8), pos Vector.+ (8, 8))
  checkCollision t@Turret {turretHealth = (Living health), turretHitTimer = timer} other
    | timer > 0 = Just t
    | isColliding (getHitBox t) (getHitBox other) && health == 1 = Just t {turretHealth = Dying 1}
    | isColliding (getHitBox t) (getHitBox other) = Just t {turretHealth = Living (health - 1), turretHitTimer = 0.1}
  checkCollision t other = Just t

instance Collision Fighter where
  getHitBox t@Fighter {fighterPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision t@Fighter {fighterHealth = (Living health), fighterHitTimer = timer} other
    | timer > 0 = Just t
    | isColliding (getHitBox t) (getHitBox other) && health == 1 = Just t {fighterHealth = Dying 1}
    | isColliding (getHitBox t) (getHitBox other) = Just t {fighterHealth = Living (health - 1), fighterHitTimer = 0.1}
  checkCollision t other = Just t