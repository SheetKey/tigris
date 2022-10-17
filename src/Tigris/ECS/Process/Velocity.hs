module Tigris.ECS.Process.Velocity where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- rhine
import FRP.Rhine

-- apecs
import Apecs

import qualified SDL


_normVelocity :: MonadIO m => SystemT' m ()
_normVelocity = cmap $ \(Velocity v) -> NormVelocity $ SDL.normalize $ fromIntegral <$> v

normVelocity :: MonadIO m => ClSFS m cl () ()
normVelocity = constMCl _normVelocity
