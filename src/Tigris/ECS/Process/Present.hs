module Tigris.ECS.Process.Present where

-- rhine
import FRP.Rhine

-- sdl
import qualified SDL


_present :: MonadIO m => SystemT' m ()
_present = do
  Renderer ren <- get global
  SDL.present ren
  SDL.rendererDrawColor ren SDL.$= SDL.V4 0 0 0 0
  SDL.clear ren

present :: MonadIO m => ClSFS m cl () ()
present = arrMCl _present
