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
import Apecs.System


updateDestPos :: Rectangle CInt -> Rectangle CInt -> Rectangle CInt
updateDestPos (Rectangle (P (V2 x y)) wh) (Rectangle (P (V2 cx cy)) _)
  = Rectangle (P (V2 (x - cx) (y - cy))) wh

_updateDestination :: MonadIO m => SystemT' m ()
_updateDestination = do
  Camera cam <- get global
  cmapIf (\(Position pos) -> intersectRects pos cam)
    $ \(Position pos) -> Destination $ updateDestPos pos cam
      
updateDestination :: MonadIO m => ClSFS m cl () ()
updateDestination = constMCl _updateDestination
