module Tigris.ECS.Process.View where

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.OpenGL.Matrix

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

-- linear
import Linear

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

_view :: MonadIO m => SystemT' m ()
_view = cmapM_ $ \(Player, Position (V4 _ n _ _)) -> do
  let vm = viewMatrix n (V3 0 2 5)
  set global $ View vm
  GLBuffers (_,_,_, program) <- get global
  liftIO $ do 
    GL.currentProgram GL.$= Just program
    loc <- GL.get . GL.uniformLocation program $ "view"
    (toMatrix vm :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform loc GL.$=)
    GL.currentProgram GL.$= Nothing

view :: MonadIO m => ClSFS m cl () ()
view = constMCl _view
