{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.ECS.Process.WantLeftClick where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.ECS.Process.MousePosition

-- sdl
import qualified SDL

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- apecs
import Apecs

-- rhine
import FRP.Rhine hiding (get)

-- contianers
import qualified Data.IntMap.Strict as M

-- linear
import Linear

-- base
import Foreign.C.Types (CInt (..))


type FuncMap a m b = M.IntMap (Int -> a -> SystemT' m b)

  
getFunc :: MonadIO m => Int -> FuncMap a m b -> (Int -> a -> SystemT' m b)
getFunc i m = case (m M.!? i) of
                Just f -> f
                Nothing -> error $ "'FuncMap' does not contain key " ++ show i

_useLeftClick :: MonadIO m
              => ((), SDL.InputMotion)
              -> ReaderT' (FuncMap (V3 GL.GLfloat, V3 GL.GLfloat) m ()) m ((), SDL.InputMotion)
_useLeftClick ((), last) = do
  funMap <- ask
  MouseLeftClick click <- lift $ get global
  case (last, click) of
    -- If previously released and currently released do nothing.
    (SDL.Released, Nothing) -> return ((), SDL.Released)
    -- If previously pressed and no further input, mouse is being held down.
    (SDL.Pressed, Nothing) -> do
      SDL.P (SDL.V2 (CInt x) (CInt y)) <- SDL.getAbsoluteMouseLocation
      pos1 <- lift $ _inGameMousePos (x, y)
      pos2 <- lift $ _rayMousePos (x, y)
      lift $ cmapM_ $ \(WantLeftClick i, ety :: Entity) ->
        (getFunc i funMap) (unEntity ety) (pos1, pos2)
      return ((), SDL.Pressed)
    -- On release, release.
    (_, Just (SDL.Released, _, _)) -> return ((), SDL.Released)
    -- On press, handle press.
    (_, Just (SDL.Pressed, pos1, pos2)) -> do
      lift $ cmapM_ $ \(WantLeftClick i, ety :: Entity) ->
        (getFunc i funMap) (unEntity ety) (pos1, pos2)
      return ((), SDL.Pressed)

useLeftClickR :: MonadIO m => ClSFSR (FuncMap (V3 GL.GLfloat, V3 GL.GLfloat) m ()) m cl () ()
useLeftClickR = feedback SDL.Released $ arrMCl _useLeftClick

useLeftClick :: MonadIO m => FuncMap (V3 GL.GLfloat, V3 GL.GLfloat) m () -> ClSFS m cl () ()
useLeftClick = runReaderS_ useLeftClickR
