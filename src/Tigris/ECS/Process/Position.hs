{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Tigris.ECS.Process.Position where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- rhine
import FRP.Rhine hiding ((^+^))

-- apecs
import Apecs

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

nextPosition :: V3 GL.GLfloat -> V3 GL.GLfloat -> GL.GLfloat -> Double -> V3 GL.GLfloat
nextPosition p v s dT = p ^+^ ( v ^* (s * realToFrac dT) ) 

setX0 :: V3 GL.GLfloat -> V3 GL.GLfloat
setX0 (V3 _ y z) = V3 0 y z

setZ0 :: V3 GL.GLfloat -> V3 GL.GLfloat
setZ0 (V3 x y _) = V3 x y 0

_setPosition :: MonadIO m => Double -> SystemT' m ()
_setPosition dT = cmap $ \(Position (V4 _ n _ _), Velocity v, Speed s) ->
  Position $ V4
  n
  (nextPosition n v s dT)
  (nextPosition n (setZ0 v) s dT)
  (nextPosition n (setX0 v) s dT)

setPosition :: (MonadIO m, (Diff (Time cl)) ~ Double) => ClSFS m cl () ()
setPosition = sinceLastS >>> arrMCl _setPosition
