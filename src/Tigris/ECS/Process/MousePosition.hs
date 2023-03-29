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
import Data.Int
import Control.Monad ((>=>))
import Foreign.C.Types


_mouseZCoord :: MonadIO m => (Int32, Int32) -> SystemT' m (V3 GL.GLfloat)
_mouseZCoord (mx, my) = do
  Window win <- get global
  V2 _ (CInt height) <- SDL.get $ SDL.windowSize win
  let winX = mx
      winY = abs (height - my)
  z <- liftIO $ alloca $ \ptr -> do
    let pdata = GL.PixelData GL.DepthComponent GL.Float ptr
    GL.readPixels (GL.Position (fromIntegral winX) (fromIntegral winY)) (GL.Size 1 1) pdata
    peek ptr
  return $ V3 (fromIntegral winX) (fromIntegral winY) z
        
mouseZCoord :: MonadIO m => ClSFS m cl (Int32, Int32) (V3 GL.GLfloat)
mouseZCoord = arrMCl _mouseZCoord

_screenToGameCoord :: MonadIO m => V3 GL.GLfloat -> SystemT' m (V3 GL.GLfloat)
_screenToGameCoord (V3 x y z) = do
  View view <- get global
  Projection proj <- get global
  view' :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix $ ((fmap . fmap) float2Double) view
  proj' :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix $ ((fmap . fmap) float2Double) proj
  model :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix identity
  vp <- GL.get GL.viewport
  GL.Vertex3 x' y' z' <- liftIO $ GL.unProject
                               (GL.Vertex3 (float2Double x) (float2Double y) (float2Double z))
                               view'
                               proj'
                               vp
  return $ double2Float <$> (V3 x' y' z')
  
screenToGameCoord :: MonadIO m => ClSFS m cl (V3 GL.GLfloat) (V3 GL.GLfloat)
screenToGameCoord = arrMCl _screenToGameCoord

_inGameMousePos :: MonadIO m => (Int32, Int32) -> SystemT' m (V3 GL.GLfloat)
_inGameMousePos = _mouseZCoord >=> _screenToGameCoord

inGameMousePos :: MonadIO m => ClSFS m cl (Int32, Int32) (V3 GL.GLfloat)
inGameMousePos = arrMCl _inGameMousePos

_mousePos :: MonadIO m => SystemT' m (V3 GL.GLfloat)
_mousePos = do
  SDL.P (SDL.V2 (CInt _mx) (CInt _my)) <- SDL.getAbsoluteMouseLocation
  _inGameMousePos (_mx, _my)
