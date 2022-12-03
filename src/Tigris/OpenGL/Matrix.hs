module Tigris.OpenGL.Matrix where

-- base
import Foreign.Storable
import Foreign.Ptr

-- linear
import Linear.V3
import Linear.Matrix
import Linear.Epsilon
import Linear.Projection

-- opengl
import qualified Graphics.Rendering.OpenGL as GL
  

-- camera position matrix
view :: (Epsilon a, Floating a) => V3 a -> V3 a -> M44 a
view center constant = lookAt eye center up
  where
    eye = constant + center
    up = V3 0 1 0

-- projectoin matrix
projection :: (Floating a) => a -> a -> a -> M44 a
projection fov winWidth winHeight = perspective fov (winWidth / winHeight) 0.1 100.0

-- convert to opengl matrix
toMatrix :: (GL.Matrix m, GL.MatrixComponent c) => M44 c -> IO (m c)
toMatrix mr =
  let mc = transpose mr
  in GL.withNewMatrix GL.ColumnMajor (flip poke mc . castPtr)
