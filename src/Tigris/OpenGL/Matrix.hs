module Tigris.OpenGL.Matrix where

-- base
import Foreign.Storable
import Foreign.Ptr

-- linear
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Vector
import Linear.Epsilon
import Linear.Projection

-- opengl
import qualified Graphics.Rendering.OpenGL as GL
  
-- world space
modelMatrix :: Num a => V3 a -> M44 a
modelMatrix = mkTransformationMat identity

-- camera position matrix
viewMatrix :: (Epsilon a, Floating a) => V3 a -> V3 a -> M44 a
viewMatrix center constant = lookAt eye center up
  where
    eye = constant + center
    up = V3 0 1 0

-- a view matrix that makes vertical things look better
customViewMatrix :: Fractional a => M44 a -> M44 a
customViewMatrix m@(V4 (V4 _ a _ _) (V4 _ b _ _) (V4 _ c _ _) (V4 _ d _ _)) =
  m !+! (V4 a' b' c' d')
  where a' = V4 0 ((0.001 * a) - a) 0 0
        b' = V4 0 (((0.001 * b) + 1) - b) 0 0
        c' = V4 0 ((0.001 * c) - c) 0 0
        d' = V4 0 ((0.001 * d) - d) 0 0

-- projectoin matrix
projectionMatrix :: (Floating a) => a -> a -> a -> M44 a
projectionMatrix fov winWidth winHeight = perspective fov (winWidth / winHeight) 0.2 10000.0

-- convert to opengl matrix
toMatrix :: (GL.Matrix m, GL.MatrixComponent c) => M44 c -> IO (m c)
toMatrix mr =
  let mc = transpose mr
  in GL.withNewMatrix GL.ColumnMajor (flip poke mc . castPtr)
