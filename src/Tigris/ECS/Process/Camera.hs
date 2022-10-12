module Tigris.ECS.Process.Camera where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics

-- rhine
import FRP.Rhine

-- apecs
import Apecs


moveCamera :: Rectangle CInt -> Rectangle CInt -> Rectangle CInt
moveCamera c@(Rectangle _ (V2 w h)) (Rectangle (P (V2 x y)) _)
  = Rectangle (x - (w `div` 2)) (y - (h `div` 2) w h

_updateCamera :: SystemT' m ()
_updateCamera = cmapM_ $ \(Player, Position pos) ->
  do
    modify global $ \(Camera c) -> Camera $ moveCamera c pos

updateCamera :: ClSFS m cl () ()
updateCamera = constMCl _updateCamera
