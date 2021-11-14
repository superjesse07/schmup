module Collision where

import Cargo
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace (trace)
import Fighter
import GameState
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vector
import Gun
import Model
import Player
import Turret

isColliding :: (Vector, Vector) -> (Vector, Vector) -> Bool
isColliding ((leftA, bottomA), (rightA, topA)) ((leftB, bottomB), (rightB, topB)) = leftA < rightB && rightA > leftB && bottomA < topB && topA > bottomB

handleCollision :: GameState -> GameState
handleCollision gstate@PlayingState {player = p, turrets = turrets, fighters = fighters, bullets = bullets, cargoShips = cargoShips, cargoDrops = cargoDrops}
  | null collidingCargo = finalGState
  | otherwise = finalGState {player = (player finalGState) {playerWeapon = getDefaultGun (cargoType (head collidingCargo)), playerPowerup = 10}}
  where
    collidedPlayer = collideWith (p) $ filter (not . isPlayerBullet) bullets
    collidedFighters = mapMaybe (`collideWith` filter isPlayerBullet bullets) fighters
    collidedTurret = mapMaybe (`collideWith` filter isPlayerBullet bullets) turrets
    collidedCargoShips = mapMaybe (`collideWith` filter isPlayerBullet bullets) cargoShips
    collidedBulletsEnemy = mapMaybe (`collideWith` [p]) $ filter (not . isPlayerBullet) bullets
    collidedBulletsPlayer = mapMaybe ((`collideWithMaybe` cargoShips) . (`collideWithMaybe` fighters) . (`collideWith` turrets)) $ filter isPlayerBullet bullets
    collidingCargo = filter (isColliding (getHitBox p) . getHitBox) cargoDrops
    collidedCargo = mapMaybe (`collideWith` [p]) cargoDrops
    finalGState =
      gstate
        { player = fromMaybe p collidedPlayer,
          bullets = collidedBulletsEnemy ++ collidedBulletsPlayer,
          fighters = collidedFighters,
          turrets = collidedTurret,
          cargoShips = collidedCargoShips,
          cargoDrops = collidedCargo
        }

collideWith :: Collision a => Collision b => a -> [b] -> Maybe a
collideWith a = collideWithMaybe (Just a)

collideWithMaybe :: Collision a => Collision b => Maybe a -> [b] -> Maybe a
collideWithMaybe Nothing _ = Nothing
collideWithMaybe (Just collider) [] = Just collider
collideWithMaybe (Just collider) (x : xs) = collideWithMaybe (checkCollision collider x) xs

class Collision a where
  getHitBox :: a -> (Vector, Vector)
  checkCollision :: Collision b => a -> b -> Maybe a

instance Collision Player where
  getHitBox p@Player {playerPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision p@Player {playerState = (Living health), playerHitTimer = timer} other
    | timer > 0 = Just p
    | isColliding (getHitBox p) (getHitBox other) && health == 1 = Just p {playerState = Dying 3}
    | isColliding (getHitBox p) (getHitBox other) = Just p {playerState = Living (health - 1), playerHitTimer = 0.2}
  checkCollision p other = Just p

instance Collision Projectile where
  getHitBox (DefaultProjectile _ _ pos) = (pos Vector.- (1, 1), pos Vector.+ (1, 1))
  getHitBox (BurstProjectile _ _ pos) = (pos Vector.- (1, 1), pos Vector.+ (1, 1))
  getHitBox (LaserProjectile _ pos _) = (pos Vector.- (0, 1), pos Vector.+ (1000, 1))
  checkCollision this@LaserProjectile {} _ = Just this
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

instance Collision CargoShip where
  getHitBox t@CargoShip {cargoShipPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision t@CargoShip {cargoShipHealth = (Living health), cargoShipHitTimer = timer} other
    | timer > 0 = Just t
    | isColliding (getHitBox t) (getHitBox other) && health == 1 = Just t {cargoShipHealth = Dying 1}
    | isColliding (getHitBox t) (getHitBox other) = Just t {cargoShipHealth = Living (health - 1), cargoShipHitTimer = 0.1}
  checkCollision t other = Just t

instance Collision CargoPickup where
  getHitBox t@CargoPickup {cargoPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision this other
    | isColliding (getHitBox this) (getHitBox other) = Nothing
    | otherwise = Just this
