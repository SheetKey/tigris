{- |
Entities that can move should not be allowed beyond
the bounds of the tilemap.
-}

{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Process.BorderCollision where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.Graphics 
import Tigris.ECS.Process.Position

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

-- | Detect the collision of a movable entity.
--   collision based on next predicted position in order to
--   avoid strange behavior, i.e., getting stuck or clipping.
--   As such, `normVelocity` should likely be called before and
--   after this process is called, all of which happend before
--   `setPosition`.
_borderCollision :: MonadIO m => Double -> SystemT' m ()
_borderCollision dT = do
  TileMapSize (V2 tmw tmh) <- get global
  cmap $ \(Position p, NormVelocity v, Speed s, Velocity (V2 vx vy)) ->
    let Rectangle (P (V2 x y)) (V2 w h) = nextPosition p v s dT
    in case (x < 0, y < 0, x+w > tmw, y+h > tmh) of
      (True, True, _   , _   ) -> Velocity $ V2 0 0
      (_   , _   , True, True) -> Velocity $ V2 0 0
      (True, _   , _   , True) -> Velocity $ V2 0 0
      (_   , True, True, _   ) -> Velocity $ V2 0 0
      (True, _   , _   , _   ) -> Velocity $ V2 0 vy
      (_   , _   , True, _   ) -> Velocity $ V2 0 vy
      (_   , True, _   , _   ) -> Velocity $ V2 vx 0 
      (_   , _   , _   , True) -> Velocity $ V2 vx 0
      _                        -> Velocity $ V2 vx vy

borderCollision :: (MonadIO m, Diff (Time cl) ~ Double) => ClSFS m cl () ()
borderCollision = sinceLastS >>> arrMCl _borderCollision
