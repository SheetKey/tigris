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
import Tigris.ECS.Process.Position
import Tigris.Graphics

-- rhine
import FRP.Rhine hiding (get)

cellWH :: CInt
cellWH = 128

-- | Initiallizes the collider cell for all entities that have a position.
--   This show likely be deleted later since stationary entities will
--   be part of a KDTree.
--   Alternatively, this should ensure entities have a speed component.
_initColliderCell :: MonadIO m => SystemT' m ()
_initColliderCell = do
  GridSize (V2 nCols _) <- get global
  cmap $ \(Position (Rectangle (P (V2 x y)) (V2 w h))) ->
    let nw = 2 ^ (x       `div` cellWH + nCols * (y       `div` cellWH))
        ne = 2 ^ ((x + w) `div` cellWH + nCols * (y       `div` cellWH))
        se = 2 ^ ((x + w) `div` cellWH + nCols * ((y + h) `div` cellWH))
        sw = 2 ^ (x       `div` cellWH + nCols * ((y + h) `div` cellWH))
    in ColliderCell $ nw .|. ne .|. se .|. sw

-- | Calculates a movable entities next collision cell.
_colliderCell :: MonadIO m => Double -> SystemT' m ()
_colliderCell dT = do
  GridSize (V2 nCols _) <- get global
  cmap $ \(Position p, NormVelocity v, Speed s) -> 
    let (Rectangle (P (V2 x y)) (V2 w h)) = nextPosition p v s dT
        nw = 2 ^ (x       `div` cellWH + nCols * (y       `div` cellWH))
        ne = 2 ^ ((x + w) `div` cellWH + nCols * (y       `div` cellWH))
        se = 2 ^ ((x + w) `div` cellWH + nCols * ((y + h) `div` cellWH))
        sw = 2 ^ (x       `div` cellWH + nCols * ((y + h) `div` cellWH))
    in ColliderCell $ nw .|. ne .|. se .|. sw


-- | Collider all movable entities with a collider cell.
_collide :: MonadIO m => SystemT' m [(Int, Int)]
_collide = do
  store :: Storage (NormVelocity, ColliderCell) <- getStore
  storel <- lift $ explMembers store
  let
    -- Check whether an entity collides with the remaining entities.
    check s (ety1, sl) acc = do
      let ety2 = U.head sl
          slTail = U.tail sl
      (_, ColliderCell cell1) <- lift $ explGet s ety1
      (_, ColliderCell cell2) <- lift $ explGet s ety2
      case (cell1 .&. cell2 /= 0, U.null slTail) of 
        -- Collide and no more entities left to check.
        (True, True) -> return $ U.cons (ety1, ety2) acc
        -- Collide and more entities to check.
        (True, False) -> check s (ety1, slTail) (U.cons (ety1, ety2) acc)
        -- Does not collide and no more entities to check.
        (False, True) -> return acc
        -- Does not collide and more entities to check.
        (False, False) -> check s (ety1, slTail) acc
    -- create a vector of head and tails decreasing in length; prevents redundant collision calculations.
    -- TODO: Check whether just doing things redundantly is actually more efficient than all of this mess.
    headAndTails v acc = case U.uncons v of
                           Just ht@(_, t) -> headAndTails t $ ht : acc
                           Nothing        -> acc
    -- Check all collisions
    go s sl = forM (headAndTails sl []) (flip (check s) U.empty)
  U.toList <$> U.concat <$> go store storel
