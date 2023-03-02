{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Rotation where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

-- calcAngle :: V2 CInt -> Rectangle CInt -> (CInt, CInt) -> CDouble
-- calcAngle (V2 tx ty) (Rectangle (P (V2 x y)) (V2 w h)) (xFrac, yFrac)
--   = let dx = fromIntegral (x - tx) + ((fromIntegral w) / (fromIntegral xFrac))
--         dy = fromIntegral (y - ty) + ((fromIntegral h) / (fromIntegral yFrac))
--         angle = ((atan2 dy dx) * 180 / 3.14) - 90
--     in angle
-- 
_rotAngle :: MonadIO m => Plane -> GL.GLfloat -> SystemT' m ()
_rotAngle plane ang = cmap $
  \(Rotation {..}) ->
    case plane of
      XY -> Rotation { xyangle = ang, .. }
      YZ -> Rotation { yzangle = ang, .. }
      XZ -> Rotation { xzangle = -ang, .. }

rotAngle :: MonadIO m => Plane -> ClSFS m cl GL.GLfloat ()
rotAngle plane = arrMCl $ _rotAngle plane

_rotate :: MonadIO m => SystemT' m ()
_rotate = cmap $ \(Rotation {..}) ->
  let sa = sin xyangle
      sb = sin xzangle
      sg = sin yzangle
      ca = cos xyangle
      cb = cos xzangle
      cg = cos yzangle
  in RotationMat $ (flip mkTransformationMat) zero
     (V3 (V3 (ca * cb) ((ca * sb * sg) - (sa * cg)) ((ca * sb * cg) + (sa * sg)))
         (V3 (sa * cb) ((sa * sb * sg) + (ca * cg)) ((sa * sb * cg) - (ca * sg)))
         (V3 (-sb)     (cb * sg)                    (cb * cg))
     )

rotate :: MonadIO m => ClSFS m cl () ()
rotate = constMCl _rotate
