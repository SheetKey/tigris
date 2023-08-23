{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.VoxelDraw where

-- vox-hs
import GLVoxInfo
import Gox.Type

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

_voxelDraw :: MonadIO m => SystemT' m Int
_voxelDraw = do
  Window window <- get global
  
  GLVoxelBuffers (vao, _, program) <- get global
  GL.bindVertexArrayObject GL.$= Just vao
  GL.currentProgram GL.$= Just program

  cmapM_ $ \ (Voxel info, VoxPos pos) -> do
    forM_ (layers info) $ \ (LBD {..}) -> do
      let v = V.concat [ (V.fromList . toList . point) pos
                       , V.fromList [ blockX, blockY, blockZ ]
                       , (blocks info) V.! blockIndex
                       ]
      liftIO $ do
        bufferDataWithVector v GL.ArrayBuffer GL.DynamicDraw
        -- TODO: set mvp matrices
        GL.drawArrays GL.Points 0 1

  GL.bindVertexArrayObject GL.$= Nothing
  GL.currentProgram GL.$= Nothing
