{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Rotation where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics
import Tigris.FRP

-- rhine
import FRP.Rhine

-- apecs
import Apecs


mousePosition :: MonadIO m => ClSFS m cl () (Point V2 CInt)
mousePosition = constMCl getAbsoluteMouseLocation

calcAngle :: Point V2 CInt -> Rectangle CInt -> (CInt, CInt) -> CDouble
calcAngle (P (V2 tx ty)) (Rectangle (P (V2 x y)) (V2 w h)) (xFrac, yFrac)
  = let dx = fromIntegral (x - tx) + ((fromIntegral w) / (fromIntegral xFrac))
        dy = fromIntegral (y - ty) + ((fromIntegral h) / (fromIntegral yFrac))
        angle = ((atan2 dy dx) * 180 / 3.14) - 90
    in angle

_rotateTowardsWDest :: MonadIO m => Point V2 CInt -> SystemT' m ()
_rotateTowardsWDest pnt = cmap $
  \(RToMouse, Rotation {..}, Destination dest) ->
    Rotation { angle = calcAngle pnt dest rotPntFrac, .. }
  

_rotateTowardsWODest :: MonadIO m => Point V2 CInt -> SystemT' m ()
_rotateTowardsWODest pnt = cmap $
  \(RToMouse, Rotation {..}, Position pos, Not :: Not Destination) ->
    Rotation { angle = calcAngle pnt pos rotPntFrac, .. }
  
-- add a component to determine flipping rules

rotateTowardsMouse :: MonadIO m => ClSFS m cl () ()
rotateTowardsMouse = mousePosition
                     >>> splitInput
                     >>> (arrMCl _rotateTowardsWDest) *** (arrMCl _rotateTowardsWODest)
                     >>> joinOutput