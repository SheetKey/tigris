module Tigris.ECS.Process.Frame where

-- vox-hs
import Vox.GLVoxInfo
import Vox.Gox

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.OpenGL

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

-- vector
import qualified Data.Vector.Storable as V

-- base
import Foreign.Ptr
import Data.Foldable (toList)

-- sdl
import qualified SDL

_initFrame :: MonadIO m => SystemT' m ()
_initFrame = liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

_drawFrame :: MonadIO m => SystemT' m ()
_drawFrame = do
  Window window <- get global
  SDL.glSwapWindow window

initFrame :: MonadIO m => ClSFS m cl () ()
initFrame = constMCl _initFrame

drawFrame :: MonadIO m => ClSFS m cl () ()
drawFrame = constMCl _drawFrame
