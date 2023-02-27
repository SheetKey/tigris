{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.ECS.Process.MousePosition where

-- tigris
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.OpenGL

-- apecs
import Apecs

-- rhine
import FRP.Rhine hiding (get)

-- sld
import qualified SDL

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

-- base
import GHC.Float (double2Float, float2Double)
import Foreign.Marshal.Alloc
import Foreign.Storable


_mousePosition :: MonadIO m => SystemT' m (V3 GL.GLfloat)
_mousePosition = do
  View view <- get global
  Projection proj <- get global
  view' :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix $ ((fmap . fmap) float2Double) view
  proj' :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix $ ((fmap . fmap) float2Double) proj
  model :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix identity
  vp <- GL.get GL.viewport
  SDL.P (SDL.V2 mx my) <- SDL.getAbsoluteMouseLocation
  GL.Vertex3 x y z <- liftIO $ GL.unProject
                               (GL.Vertex3 (fromIntegral mx) (fromIntegral my) 0.6)
                               view'
                               proj'
                               vp
  liftIO $ print (x,y,z)
  return $ double2Float <$> (V3 x y z)

mousePosition :: MonadIO m => ClSFS m cl () (V3 GL.GLfloat)
mousePosition = constMCl _mousePosition

_mousePosition' :: MonadIO m => m (V3 GL.GLfloat)
_mousePosition' = do
  SDL.P (SDL.V2 mx my) <- SDL.getAbsoluteMouseLocation
  z <- liftIO $ alloca $ \ptr -> do
    let pdata = GL.PixelData GL.DepthComponent GL.Float ptr
    GL.readPixels (GL.Position (fromIntegral mx) (fromIntegral my)) (GL.Size 1 1) pdata
    peek ptr
  return $ V3 (fromIntegral mx) (fromIntegral my) z
        
  
