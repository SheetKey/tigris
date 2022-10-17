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

nextPosition :: Rectangle CInt -> V2 Double -> Double -> Double -> Rectangle CInt
nextPosition p v s dT = modPntV (truncate . (*s) . (*dT) <$> v) p
  
_setPosition :: MonadIO m => Double -> SystemT' m ()
_setPosition dT = cmap $ \(Position p, NormVelocity v, Speed s) -> Position $ nextPosition p v s dT

setPosition :: (MonadIO m, (Diff (Time cl)) ~ Double) => ClSFS m cl () ()
setPosition = sinceLastS >>> arrMCl _setPosition
