module Tigris.ECS.Process.Destination where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

updateDestPos :: Rectangle CInt -> Rectangle CInt -> Rectangle CInt
updateDestPos (Rectangle (P (V2 x y)) wh) (Rectangle (P (V2 cx cy)) _)
  = Rectangle (P (V2 (x - cx) (y - cy))) wh

_updateDestination :: MonadIO m => SystemT' m ()
_updateDestination = cmapM_ $ \(Position pos, Destination _, ety) ->
  do
    Camera cam <- get global
    if intersectRects pos cam
      then do
      set ety (Destination $ updateDestPos pos cam)
      else do
      return ()
      
  
