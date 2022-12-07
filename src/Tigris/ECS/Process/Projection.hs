module Tigris.ECS.Process.Projection where

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.ECS.Clock
import Tigris.OpenGL.Matrix

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

-- linear
import Linear

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- base
import Data.Int


_projection :: MonadIO m => V2 Int32 -> SystemT' m ()
_projection (V2 w h) = do
  let pm = projectionMatrix 45 (fromIntegral w) (fromIntegral h)
  set global $ Projection pm
  GLBuffers (_,_,_, program) <- get global
  liftIO $ do
    GL.currentProgram GL.$= Just program
    loc <- GL.get . GL.uniformLocation program $ "proj"
    (toMatrix pm :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform loc GL.$=)
    GL.currentProgram GL.$= Nothing

    GL.viewport GL.$= (GL.Position 0 0, GL.Size w h)
  
  

projection :: MonadIO m => ClSFS m WindowResizeClock () ()
projection = tagS >>> arrMCl _projection
