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


type FuncMap a m b = M.IntMap (a -> SystemT' m b)

  
getFunc :: MonadIO m => Int -> FuncMap a m b -> (a -> SystemT' m b)
getFunc i m = case (m M.!? i) of
                Just f -> f
                Nothing -> error $ "'FuncMap' does not contain key " ++ show i

_useLeftClick :: MonadIO m => ReaderT' (FuncMap (V3 GL.GLfloat) m ()) m ()
_useLeftClick = do
  funMap <- ask
  MouseLeftClick click <- lift $ get global
  case click of
    (SDL.Released, _) -> return ()
    (SDL.Pressed, Just pos) -> do
      lift $ cmapM_ $ \(WantLeftClick i) -> (getFunc i funMap) pos
      lift $ set global $ MouseLeftClick (SDL.Pressed, Nothing)
    (SDL.Pressed, Nothing) -> do
      SDL.P (SDL.V2 (CInt x) (CInt y)) <- SDL.getAbsoluteMouseLocation
      pos <- lift $ _inGameMousePos (x, y)
      lift $ cmapM_ $ \(WantLeftClick i) -> (getFunc i funMap) pos

useLeftClickR :: MonadIO m => ClSFSR (FuncMap (V3 GL.GLfloat) m ()) m cl () ()
useLeftClickR = constMCl _useLeftClick

useLeftClick :: MonadIO m => FuncMap (V3 GL.GLfloat) m () -> ClSFS m cl () ()
useLeftClick = runReaderS_ useLeftClickR
