{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.ECS.Process.Draw where

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

_draw :: MonadIO m => SystemT' m ()
_draw = do
  GLBuffers (vao, vbo, _, program) <- get global
  GL.bindVertexArrayObject GL.$= Just vao
  GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
  GL.currentProgram GL.$= Just program

  let indices :: V.Vector GL.GLuint = V.fromList [0, 1, 2, 0, 2, 3]
  liftIO $ bufferDataWithVector indices GL.ElementArrayBuffer GL.DynamicDraw

  cmapM_ $ \(Size size, Model m, UV uv, rmat :: Maybe RotationMat) -> do
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
      rmatLoc <- GL.get . GL.uniformLocation program $ "rmat"
      case rmat of
        Just (RotationMat rmat') -> (toMatrix rmat' :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform rmatLoc GL.$=)
        Nothing                  -> (toMatrix identity :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform rmatLoc GL.$=)
      GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
  GL.bindVertexArrayObject GL.$= Nothing
  GL.currentProgram GL.$= Nothing
    

draw :: MonadIO m => ClSFS m cl () ()
draw = constMCl _draw
