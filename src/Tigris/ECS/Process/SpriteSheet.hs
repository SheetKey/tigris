{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.SpriteSheet where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- rhine
import FRP.Rhine

-- apecs
import Apecs


_incFrame :: MonadIO m => Double -> SystemT' m ()
_incFrame time = cmap $ \(SpriteSheet {..}) ->
  if time + accTime >= waitTime
  then
    SpriteSheet
    { colIndex = if colIndex < colMax
                 then colIndex + abs frameWidth
                 else colMin
    , accTime = 0
    , ..
    }
  else
    SpriteSheet
    { accTime = time + accTime
    , ..
    }


incFrame :: (MonadIO m, Diff (Time cl) ~ Double) => ClSFS m cl () ()
incFrame = sinceLastS >>> arrMCl _incFrame
