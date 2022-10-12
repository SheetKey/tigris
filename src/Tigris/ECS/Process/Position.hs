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

  
_setPosition :: MonadIO m => CInt -> SystemT' m ()
_setPosition dT = cmap $ \(Position p, Velocity v) -> Position (modPntV v p)

setPosition :: (MonadIO m, RealFrac (Diff (Time cl))) => ClSFS m cl () ()
setPosition = sinceLastS >>> arr (* 1000) >>> arr floor >>> arrMCl _setPosition
