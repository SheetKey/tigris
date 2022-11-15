module Tigris.ECS.Process.Collisions where

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System



_collideEntities :: MonadIO m => (Int, Int) -> 
