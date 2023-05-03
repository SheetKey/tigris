{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.ECS.Process.MousePosition where

-- tigris
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.OpenGL

-- apecs
import Apecs

-- rhine
import FRP.Rhine hiding (get, dot, (*^))

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

_planeMouseZCoord :: MonadIO m => (Int32, Int32) -> SystemT' m (V3 GL.GLfloat)
_planeMouseZCoord (mx, my) = do
  Window win <- get global
  V2 _ (CInt height) <- SDL.get $ SDL.windowSize win
  let winY = abs (height - my)
  return $ V3 (fromIntegral mx) (fromIntegral winY) 0.9998
  

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

-- | This provides the mouse position in the world using gl depth buffers.
-- I.e. this will give the mouse position on an object in the world.
-- To get the mouse position on the xz plane, use '_rayMousePos'.
_inGameMousePos :: MonadIO m => (Int32, Int32) -> SystemT' m (V3 GL.GLfloat)
_inGameMousePos = _mouseZCoord >=> _screenToGameCoord

inGameMousePos :: MonadIO m => ClSFS m cl (Int32, Int32) (V3 GL.GLfloat)
inGameMousePos = arrMCl _inGameMousePos

_planeInGameMousePos :: MonadIO m => (Int32, Int32) -> SystemT' m (V3 GL.GLfloat)
_planeInGameMousePos = _planeMouseZCoord >=> _screenToGameCoord

planeInGameMousePos :: MonadIO m => ClSFS m cl (Int32, Int32) (V3 GL.GLfloat)
planeInGameMousePos = arrMCl _planeInGameMousePos

_mousePos :: MonadIO m => SystemT' m (V3 GL.GLfloat)
_mousePos = do
  SDL.P (SDL.V2 (CInt _mx) (CInt _my)) <- SDL.getAbsoluteMouseLocation
  _inGameMousePos (_mx, _my)

_planeMousePos :: MonadIO m => SystemT' m (V3 GL.GLfloat)
_planeMousePos = do
  SDL.P (SDL.V2 (CInt _mx) (CInt _my)) <- SDL.getAbsoluteMouseLocation
  _planeInGameMousePos (_mx, _my)


-- using ray tracing
-- https://antongerdelan.net/opengl/raycasting.html
-- https://stackoverflow.com/questions/23975555/how-to-do-ray-plane-intersection


mouseRay :: MonadIO m => (Int32, Int32) -> SystemT' m (V3 GL.GLfloat, V3 GL.GLfloat)
mouseRay (mx, my) = do
  Window win <- get global
  V2 _ (CInt height) <- SDL.get $ SDL.windowSize win
  let winX = mx
      winY = abs (height - my)
  View view <- get global
  Projection proj <- get global
  view' :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix $ ((fmap . fmap) float2Double) view
  proj' :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix $ ((fmap . fmap) float2Double) proj
  model :: GL.GLmatrix GL.GLdouble <- liftIO $ toMatrix identity
  vp <- GL.get GL.viewport
  GL.Vertex3 px py pz <- liftIO $ GL.unProject
                         (GL.Vertex3 (fromIntegral winX) (fromIntegral winY) (fromIntegral 0))
                         view'
                         proj'
                         vp
  GL.Vertex3 vx vy vz <- liftIO $ GL.unProject
                         (GL.Vertex3 (fromIntegral winX) (fromIntegral winY) (fromIntegral 1))
                         view'
                         proj'
                         vp
  return $ (fmap double2Float (V3 px py pz), fmap double2Float (V3 vx vy vz))
  

rayPlaneInter :: (V3 GL.GLfloat, V3 GL.GLfloat) -> V3 GL.GLfloat
rayPlaneInter (p, v) = 
  let n = V3 0 1 0
      denom = n `dot` v
  in if (abs denom) < 0.0001
     then V3 0 0 0
     else let t = - (dot n p) / denom
         in if t < 0.0001
            then V3 1 1 1
            else p + (t *^ v)

rayPlaneInter' :: (V3 GL.GLfloat, V3 GL.GLfloat) -> V3 GL.GLfloat
rayPlaneInter' (p@(V3 _ py _), v@(V3 _ vy _)) = 
  let t = - (py / vy)
  in p + (t *^ v)
            
-- This is the best option for finding the mouse position in the xz plane.
_rayMousePos :: MonadIO m => (Int32, Int32) -> SystemT' m (V3 GL.GLfloat)
_rayMousePos = (fmap rayPlaneInter') . mouseRay
