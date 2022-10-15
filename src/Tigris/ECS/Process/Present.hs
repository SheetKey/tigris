module Tigris.ECS.Process.Present where

-- rhine
import FRP.Rhine hiding (get)

-- sdl
import qualified SDL

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- apecs
import Apecs


_present :: MonadIO m => SystemT' m ()
_present = do
  Renderer ren <- get global
  SDL.present ren
  SDL.rendererDrawColor ren SDL.$= SDL.V4 0 0 0 0
  SDL.clear ren

present :: MonadIO m => ClSFS m cl () ()
present = constMCl _present
