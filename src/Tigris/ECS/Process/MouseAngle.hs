module Tigris.ECS.Process.MouseAngle where

-- tigris
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.ECS.Process.MousePosition

-- apecs
import Apecs

-- rhine
import FRP.Rhine hiding (get)

-- sld
import qualified SDL

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear


_angleFromTo :: V3 GL.GLfloat -> V3 GL.GLfloat -> GL.GLfloat
_angleFromTo (V3 x _ z) (V3 mx _ mz) = atan2 (mz - z) (mx - x)

-- dep
_mouseAngle :: MonadIO m => SystemT' m GL.GLfloat
_mouseAngle = do
  SDL.P (SDL.V2 mx my) <- SDL.getAbsoluteMouseLocation
  Window win <- get global
  SDL.V2 ww wh <- SDL.get $ SDL.windowSize win
  let centerX = (/2) $ fromIntegral ww
      centerY = (/2) $ fromIntegral wh
      ang = atan2 ((fromIntegral my) - centerY) ((fromIntegral mx) - centerX)
  return ang

-- dep
mouseAngle :: MonadIO m => ClSFS m cl () GL.GLfloat
mouseAngle = constMCl _mouseAngle
