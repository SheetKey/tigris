module Tigris.ECS.Process.Destination where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.Graphics 

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs


updateDestPos :: Rectangle CInt -> Rectangle CInt -> Rectangle CInt
updateDestPos (Rectangle (P (V2 x y)) wh) (Rectangle (P (V2 cx cy)) _)
  = Rectangle (P (V2 (x - cx) (y - cy))) wh

-- | Update the destination rectangle for entities with a position.
--   If the position does not intersect the camera, delete the entities
--   destination component. This ensures that an entity that will not
--   be visible on screen is not rendered by the `copyAll` process.
_updateDestination :: MonadIO m => SystemT' m ()
_updateDestination = do
  Camera cam <- get global
  cmap $ \(Position pos) -> if intersectRects pos cam
                            then Just $ Destination $ updateDestPos pos cam
                            else Nothing :: Maybe Destination
      
updateDestination :: MonadIO m => ClSFS m cl () ()
updateDestination = constMCl _updateDestination
