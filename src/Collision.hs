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
  | otherwise = finalGState {player = (player finalGState) {playerWeapon = getDefaultGun (cargoType (head collidingCargo)), playerPowerup = 10}} -- if there was a cargo box picked up apply it's powerup to the player
  where
    collidedPlayer = collideWith (p) $ filter (not . isPlayerBullet) bullets -- collide the player with the enemy bullets
    collidedFighters = mapMaybe (`collideWith` filter isPlayerBullet bullets) fighters -- collide the fighters with the player bullets
    collidedTurret = mapMaybe (`collideWith` filter isPlayerBullet bullets) turrets -- collide the turrets with the player bullets
    collidedCargoShips = mapMaybe (`collideWith` filter isPlayerBullet bullets) cargoShips -- collide the cargoships with the player bullets
    collidedBulletsEnemy = mapMaybe (`collideWith` [p]) $ filter (not . isPlayerBullet) bullets -- collide the enemy bullets with the player 
    collidedBulletsPlayer = mapMaybe ((`collideWithMaybe` cargoShips) . (`collideWithMaybe` fighters) . (`collideWith` turrets)) $ filter isPlayerBullet bullets -- collide the player bullets with the enemies
    collidingCargo = filter (isColliding (getHitBox p) . getHitBox) cargoDrops -- is a list of all cargopickups that have been picked up this frame
    collidedCargo = mapMaybe (`collideWith` [p]) cargoDrops -- collide the cargoPickups with the player
    finalGState = -- the updated gamestate with all the collisions
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

collideWithMaybe :: Collision a => Collision b => Maybe a -> [b] -> Maybe a -- Recursively collides with objects
collideWithMaybe Nothing _ = Nothing
collideWithMaybe (Just collider) [] = Just collider
collideWithMaybe (Just collider) (x : xs) = collideWithMaybe (checkCollision collider x) xs

class Collision a where
  getHitBox :: a -> (Vector, Vector) -- Determines the hitbox of the object with (left,bottom) (right,top)
  checkCollision :: Collision b => a -> b -> Maybe a -- Checks if an object is colliding with another

instance Collision Player where
  getHitBox p@Player {playerPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision p@Player {playerState = (Living health), playerHitTimer = timer} other
    | timer > 0 = Just p -- Can't get hit if the hit timer is > 0
    | isColliding (getHitBox p) (getHitBox other) && health == 1 = Just p {playerState = Dying 3} -- the object dies if it's hit on it's last health
    | isColliding (getHitBox p) (getHitBox other) = Just p {playerState = Living (health - 1), playerHitTimer = 0.2} -- Remove on hp from the object
  checkCollision p other = Just p -- otherwise just do nothing

instance Collision Projectile where
  getHitBox (DefaultProjectile _ _ pos) = (pos Vector.- (1, 1), pos Vector.+ (1, 1))
  getHitBox (BurstProjectile _ _ pos) = (pos Vector.- (1, 1), pos Vector.+ (1, 1))
  getHitBox (LaserProjectile _ pos _) = (pos Vector.- (0, 1), pos Vector.+ (1000, 1))
  checkCollision this@LaserProjectile {} _ = Just this -- Don't delete the laser
  checkCollision this other
    | isColliding (getHitBox this) (getHitBox other) = Nothing -- If they are colliding delete the bullet
    | otherwise = Just this -- otherwise just do nothing

instance Collision Turret where
  getHitBox t@Turret {turretPosition = pos} = (pos Vector.- (8, 8), pos Vector.+ (8, 8))
  checkCollision t@Turret {turretHealth = (Living health), turretHitTimer = timer} other
    | timer > 0 = Just t -- Can't get hit if the hit timer is > 0
    | isColliding (getHitBox t) (getHitBox other) && health == 1 = Just t {turretHealth = Dying 1} -- the object dies if it's hit on it's last health
    | isColliding (getHitBox t) (getHitBox other) = Just t {turretHealth = Living (health - 1), turretHitTimer = 0.1} -- Remove on hp from the object
  checkCollision t other = Just t -- otherwise just do nothing

instance Collision Fighter where
  getHitBox t@Fighter {fighterPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision t@Fighter {fighterHealth = (Living health), fighterHitTimer = timer} other
    | timer > 0 = Just t -- Can't get hit if the hit timer is > 0
    | isColliding (getHitBox t) (getHitBox other) && health == 1 = Just t {fighterHealth = Dying 1} -- the object dies if it's hit on it's last health
    | isColliding (getHitBox t) (getHitBox other) = Just t {fighterHealth = Living (health - 1), fighterHitTimer = 0.1} -- Remove on hp from the object
  checkCollision t other = Just t

instance Collision CargoShip where
  getHitBox t@CargoShip {cargoShipPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision t@CargoShip {cargoShipHealth = (Living health), cargoShipHitTimer = timer} other
    | timer > 0 = Just t -- Can't get hit if the hit timer is > 0
    | isColliding (getHitBox t) (getHitBox other) && health == 1 = Just t {cargoShipHealth = Dying 1} -- the object dies if it's hit on it's last health
    | isColliding (getHitBox t) (getHitBox other) = Just t {cargoShipHealth = Living (health - 1), cargoShipHitTimer = 0.1}-- Remove on hp from the object
  checkCollision t other = Just t -- otherwise just do nothing

instance Collision CargoPickup where
  getHitBox t@CargoPickup {cargoPosition = pos} = (pos Vector.- (4, 4), pos Vector.+ (4, 4))
  checkCollision this other
    | isColliding (getHitBox this) (getHitBox other) = Nothing
    | otherwise = Just this -- otherwise just do nothing
