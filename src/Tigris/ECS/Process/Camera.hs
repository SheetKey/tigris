{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Process.Camera where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics
import Tigris.ECS.Clock

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs


moveCamera :: Rectangle CInt -> Rectangle CInt -> Rectangle CInt
moveCamera c@(Rectangle _ (V2 w h)) (Rectangle (P (V2 x y)) _)
  = mkRect (x - (w `div` 2)) (y - (h `div` 2)) w h

_updateCamera :: MonadIO m => SystemT' m ()
_updateCamera = cmapM_ $ \(Player, Position pos) ->
  do
    modify global $ \(Camera c) -> Camera $ moveCamera c pos

updateCamera :: MonadIO m => ClSFS m cl () ()
updateCamera = constMCl _updateCamera

bound :: Rectangle CInt -> V2 CInt -> Rectangle CInt
bound (Rectangle (P (V2 x y)) wh@(V2 w h)) (V2 gw gh)
  = Rectangle (P (V2 nx ny)) wh
  where nx | x < 0 = 0
           | x > gw - w = gw - w
        ny | y < 0 = 0
           | y > gh - h = gh - h

_cameraBounding :: MonadIO m => SystemT' m ()
_cameraBounding = do
  TileMapSize s <- get global
  modify global $ \(Camera c) -> Camera $ bound c s

cameraBounding :: MonadIO m => ClSFS m cl () ()
cameraBounding = constMCl _cameraBounding

_cameraSizeOnWindowResize :: MonadIO m => V2 CInt -> SystemT' m ()
_cameraSizeOnWindowResize wh = modify global $ \(Camera c) -> Camera $ modSizeV wh c

cameraSizeOnWindowResize :: MonadIO m => ClSFS m WindowResizeClock () ()
cameraSizeOnWindowResize = tagS >>> arrMCl _cameraSizeOnWindowResize

cameraProcess
  :: ( MonadIO m
     , Clock (SystemT World m) cl
     , GetClockProxy cl
     , cl ~ In cl, cl ~ Out cl
     , Time cl ~ Time WindowResizeClock
     , Time cl ~ Time (Out cl)
     , Time cl ~ Time (In  cl)
     )
  => SNS m (ParClockS m WindowResizeClock cl) () ()
cameraProcess = Parallel (Synchronous cameraSizeOnWindowResize) (Synchronous $ updateCamera >>> cameraBounding)
