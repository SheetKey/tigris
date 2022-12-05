module Tigris.ECS.Process.Projection where

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.OpenGL.Matrix

-- rhine
import Rhine


_projection :: MonadIO m => V2 GL.GLfloat -> SystemT' m ()
_projection (V2 w h) = set global $ Projection $ projectionMatrix 90 w h

projection :: MonadIO m => ClSFS m WindowResizeClock () ()
projection = tagS >>> arrMCl _projection
