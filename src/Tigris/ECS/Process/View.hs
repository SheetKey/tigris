module Tigris.ECS.Process.View where

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.OpenGL.Matrix

-- rhine
import Rhine

_view :: MonadIO m => SystemT' m ()
_view = cmapM_ $ \(Player, Position (V4 _ n _ _)) -> do
  set global $ View viewMatrix n (V3 0 2 5)

view :: MonadIO m => ClSFS m cl () ()
view = constMCL _view
