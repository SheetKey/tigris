{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.ECS.Process.Draw where

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
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
import Control.Monad.Zip (mzipWith)

-- sdl
import qualified SDL

_loadVBOEBO :: MonadIO m => SystemT' m Int
_loadVBOEBO = do
  GLBuffers (vao, vbo, ebo, program) <- get global
  GL.bindVertexArrayObject GL.$= Just vao

  (vertices :: V.Vector GL.GLfloat, indices :: V.Vector GL.GLuint) <- (flip cfold) (V.empty, V.empty) $ \(v,i) (Size size, Model m, UV uv) ->
    let (V4 tl  tr  br  bl ) = (V.fromList . toList . (m !*) . point) <$> size
        (V4 tl' tr' br' bl') = (V.fromList . toList) <$> uv
        idx = (`div` 6) . fromIntegral $ V.length v
    --in ( V.concat [v, tl, tl', tr, tr', br, br', bl, bl']
    --   , i V.++ V.fromList [idx, (idx+1), (idx+2), idx, (idx+2), (idx+3)]
    --   )
    in ( v V.++ V.concat [tl, tl', tr, tr', br, br', bl, bl']
       , i V.++ V.fromList [idx, (idx+1), (idx+2), idx, (idx+2), (idx+3)]
       )


  liftIO $ do
    bufferDataWithVector vertices GL.ArrayBuffer GL.DynamicDraw
    bufferDataWithVector indices GL.ElementArrayBuffer GL.DynamicDraw
  
    GL.bindVertexArrayObject GL.$= Nothing

  return $ V.length indices 


_draw :: MonadIO m => Int -> SystemT' m ()
_draw l = do
  GLBuffers (vao, vbo, ebo, program) <- get global
  Window window <- get global

  GL.bindVertexArrayObject GL.$= Just vao
  GL.currentProgram GL.$= Just program

  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  liftIO $ GL.drawElements GL.Triangles (fromIntegral l) GL.UnsignedInt nullPtr

  SDL.glSwapWindow window

  GL.bindVertexArrayObject GL.$= Nothing
  GL.currentProgram GL.$= Nothing


draw :: MonadIO m => ClSFS m cl () ()
draw = constMCl _loadVBOEBO >>> arrMCl _draw

_altDraw :: MonadIO m => SystemT' m ()
_altDraw = do
  Window window <- get global

  GLBuffers (vao, vbo, ebo, program) <- get global
  GL.bindVertexArrayObject GL.$= Just vao
  GL.currentProgram GL.$= Just program


  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  let indices :: V.Vector GL.GLuint = V.fromList [0, 1, 2, 0, 2, 3]
  liftIO $ bufferDataWithVector indices GL.ElementArrayBuffer GL.DynamicDraw

  cmapM_ $ \(Size size, Model m, UV uv) -> do
    let (V4 tl  tr  br  bl ) = (V.fromList . toList . point) <$> size
        (V4 tl' tr' br' bl') = (V.fromList . toList) <$> uv
        vertices = V.concat [tl, tl', tr, tr', br, br', bl, bl']
    --let v1 :: V4 (V.Vector GL.GLfloat) = (V.fromList . toList) <$> size
    --    v2 :: V4 (V.Vector GL.GLfloat) = (V.fromList . toList) <$> uv
    --    vertices = foldr1 (V.++) $ mzipWith (V.++) v1 v2
    liftIO $ do
      bufferDataWithVector vertices GL.ArrayBuffer GL.DynamicDraw
      modelLoc <- GL.get . GL.uniformLocation program $ "model"
      (toMatrix m :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform modelLoc GL.$=)
      GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  SDL.glSwapWindow window

  GL.bindVertexArrayObject GL.$= Nothing
  GL.currentProgram GL.$= Nothing
    

altDraw :: MonadIO m => ClSFS m cl () ()
altDraw = constMCl _altDraw
