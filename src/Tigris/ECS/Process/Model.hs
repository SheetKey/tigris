module Tigris.ECS.Process.Model where

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.OpenGL.Matrix

-- rhine
import Rhine

_model :: MonadIO m => SystemT' m ()
_model = cmap $ \(Position (V4 _ n _ _)) -> Model $ modelMatrix n


model :: MonadIO m => ClSFS m cl () ()
model = constMCl _model
