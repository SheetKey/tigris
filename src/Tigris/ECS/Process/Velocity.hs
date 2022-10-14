module Tigris.ECS.Process.Velocity where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics

-- rhine
import FRP.Rhine hiding (normalize)

-- apecs
import Apecs


_normVelocity :: MonadIO m => SystemT' m ()
_normVelocity = cmap $ \(Velocity v) -> NormVelocity $ normalize $ fromIntegral <$> v
