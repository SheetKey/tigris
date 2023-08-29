{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.TreeDraw where

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.OpenGL

-- vox-hs
import Vox.Tree

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- base
import Foreign.Ptr
import Data.Foldable (toList)

-- sdl
import qualified SDL

_drawTree :: MonadIO m => SystemT' m ()
_drawTree = do
  GLTreeBuffers (vao, vbo, _, program) <- get global
  GL.bindVertexArrayObject GL.$= Just vao
  GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
  GL.currentProgram GL.$= Just program

  cmapM_ $ \(TreeModel rTree) -> do
    V.forM_ (curves rTree) $ \ RCurve {..} -> liftIO $ do
      bufferDataWithVector indices GL.ElementArrayBuffer GL.DynamicDraw
      bufferDataWithVector vertices GL.ArrayBuffer GL.DynamicDraw

      GL.drawElements GL.Triangles (fromIntegral $ VS.length indices) GL.UnsignedInt nullPtr
      
  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
  GL.bindVertexArrayObject GL.$= Nothing
  GL.currentProgram GL.$= Nothing

drawTree :: MonadIO m => ClSFS m cl () ()
drawTree = constMCl _drawTree
