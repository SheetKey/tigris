{- |
Entities that can move should not be allowed beyond
the bounds of the tilemap.
-}

{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Process.BorderCollision where

-- -- mylib
-- import Tigris.ECS.System
-- import Tigris.ECS.Components
-- 
-- -- rhine
-- import FRP.Rhine hiding (get)
-- 
-- -- apecs
-- import Apecs

-- -- | Detect the collision of a movable entity.
-- --   collision based on next predicted position in order to
-- --   avoid strange behavior, i.e., getting stuck or clipping.
-- --   As such, `normVelocity` should likely be called before and
-- --   after this process is called, all of which happend before
-- --   `setPosition`.
-- _borderCollision :: MonadIO m => SystemT' m ()
-- _borderCollision = do
--   TileMapSize (V2 tmw tmh) <- get global
--   cmap $ \(Position (V4 l n@(Rectangle (P (V2 x y)) (V2 w h)) nx ny)) ->
--     case (x < 0, y < 0, x+w > tmw, y+h > tmh) of
--       (True, True, _   , _   ) -> Position $ V4 l l l l -- Velocity $ V2 0 0
--       (_   , _   , True, True) -> Position $ V4 l l l l -- Velocity $ V2 0 0
--       (True, _   , _   , True) -> Position $ V4 l l l l -- Velocity $ V2 0 0
--       (_   , True, True, _   ) -> Position $ V4 l l l l -- Velocity $ V2 0 0
--       (True, _   , _   , _   ) -> Position $ V4 l ny l l -- Velocity $ V2 0 vy
--       (_   , _   , True, _   ) -> Position $ V4 l ny l l -- Velocity $ V2 0 vy
--       (_   , True, _   , _   ) -> Position $ V4 l nx l l -- Velocity $ V2 vx 0 
--       (_   , _   , _   , True) -> Position $ V4 l nx l l -- Velocity $ V2 vx 0
--       _                        -> Position $ V4 l n nx ny -- Velocity $ V2 vx vy
-- 
-- borderCollision :: MonadIO m => ClSFS m cl () ()
-- borderCollision = constMCl _borderCollision
