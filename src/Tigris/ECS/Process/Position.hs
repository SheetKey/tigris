{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Tigris.ECS.Process.Position where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics

-- rhine
import FRP.Rhine

-- apecs
import Apecs

  
_setPosition :: MonadIO m => Double -> SystemT' m ()
_setPosition dT = cmap $ \(Position p, NormVelocity v, Speed s) ->
  Position (modPntV (truncate . (*s) . (*dT) <$> v) p)

setPosition :: (MonadIO m, (Diff (Time cl)) ~ Double) => ClSFS m cl () ()
setPosition = sinceLastS >>> arrMCl _setPosition
