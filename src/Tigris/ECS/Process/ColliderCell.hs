{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.ECS.Process.ColliderCell where

-- base
import Data.Bits
import Control.Monad (forM)

-- apecs
import Apecs
import Apecs.Core

-- vectors used by apecs
import qualified Data.Vector.Unboxed as U

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System

-- rhine
import FRP.Rhine hiding (get)

-- cellWH :: CInt
-- cellWH = 128
-- 
-- nColsM :: MonadIO m => SystemT' m CInt
-- nColsM = do
--   TileMapSize (V2 x _) <- get global
--   return $ x `div` cellWH
-- 
-- -- | Initiallizes the collider cell for all entities that have a position.
-- --   This show likely be deleted later since stationary entities will
-- --   be part of a KDTree.
-- --   Alternatively, this should ensure entities have a speed component.
-- _initColliderCell :: MonadIO m => SystemT' m ()
-- _initColliderCell = do
--   nCols <- nColsM
--   cmap $ \(Position (V4 _ (Rectangle (P (V2 x y)) (V2 w h)) _ _)) ->
--     let nw = 2 ^ (x       `div` cellWH + nCols * (y       `div` cellWH))
--         ne = 2 ^ ((x + w) `div` cellWH + nCols * (y       `div` cellWH))
--         se = 2 ^ ((x + w) `div` cellWH + nCols * ((y + h) `div` cellWH))
--         sw = 2 ^ (x       `div` cellWH + nCols * ((y + h) `div` cellWH))
--     in ColliderCell $ nw .|. ne .|. se .|. sw
-- 
-- -- | Calculates a movable entities next collision cell.
-- _colliderCell :: MonadIO m => SystemT' m ()
-- _colliderCell = do
--   nCols <- nColsM
--   cmap $ \(Position (V4 _ (Rectangle (P (V2 x y)) (V2 w h)) _ _)) -> 
--     let nw = 2 ^ (x       `div` cellWH + nCols * (y       `div` cellWH))
--         ne = 2 ^ ((x + w) `div` cellWH + nCols * (y       `div` cellWH))
--         se = 2 ^ ((x + w) `div` cellWH + nCols * ((y + h) `div` cellWH))
--         sw = 2 ^ (x       `div` cellWH + nCols * ((y + h) `div` cellWH))
--     in ColliderCell $ nw .|. ne .|. se .|. sw
-- 
-- colliderCell :: MonadIO m => ClSFS m cl () ()
-- colliderCell = constMCl _colliderCell
-- 
-- 
-- 
-- -- | Collider all movable entities with a collider cell.
-- _collide :: MonadIO m => SystemT' m [(Int, Int)]
-- _collide = do
--   store :: Storage (Velocity, ColliderCell) <- getStore
--   storel <- lift $ explMembers store
--   let
--     -- Check whether an entity collides with the remaining entities.
--     check s (ety1, sl) acc = do
--       if U.null sl
--         then return acc
--         else let ety2 = U.head sl
--                  slTail = U.tail sl
--              in do (_, ColliderCell cell1) <- lift $ explGet s ety1
--                    (_, ColliderCell cell2) <- lift $ explGet s ety2
--                    if cell1 .&. cell2 /= 0
--                      then check s (ety1, slTail) (U.cons (ety1, ety2) acc)
--                      else check s (ety1, slTail) acc
-- 
--     -- create a vector of head and tails decreasing in length; prevents redundant collision calculations.
--     -- TODO: Check whether just doing things redundantly is actually more efficient than all of this mess.
--     headAndTails v acc = case U.uncons v of
--                            Just ht@(_, t) -> headAndTails t $ ht : acc
--                            Nothing        -> acc
--     -- Check all collisions
--     go s sl = forM (headAndTails sl []) (flip (check s) U.empty)
--   U.toList <$> U.concat <$> go store storel
-- 
-- 
-- _handleCollision :: MonadIO m => [(Int, Int)] -> SystemT' m ()
-- _handleCollision [] = return ()
-- _handleCollision (x:xs) = do
--   set global $ Collisions x
--   _handleCollision xs
--   
-- collide :: MonadIO m => ClSFS m cl () ()
-- collide = constMCl _collide >>> arrMCl _handleCollision
