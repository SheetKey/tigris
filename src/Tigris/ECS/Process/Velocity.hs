module Tigris.ECS.Process.Velocity where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics

-- rhine
import FRP.Rhine

-- apecs
import Apecs

import qualified SDL


_normVelocity :: MonadIO m => SystemT' m ()
_normVelocity = cmap $ \(Velocity v) -> NormVelocity $ SDL.normalize $ fromIntegral <$> v

normVelocity :: MonadIO m => ClSFS m cl () ()
normVelocity = constMCl _normVelocity
