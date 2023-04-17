{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Shoot where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

bullet :: MonadIO m => V3 GL.GLfloat -> ProjStats -> SystemT' m ()
bullet dir (ProjStats {..}) = let p = V3 0 0 0 in newEntity_
  ( Size (V4 (V3 (-8) 0 (-8)) (V3 8 0 (-8)) (V3 8 0 8) (V3 (-8) 0 8))
  , Position (V4 p p p p)
  , SpriteSheet 1 (4096) 0 0 (34 * 4) 34 34 1 2 0
  , speed
  , Velocity (One, One)
  )

_shoot :: MonadIO m => SystemT' m ()
_shoot = cmapM $ \(Shoot dir, ps@(ProjStats {..})) -> do
  bullet dir ps
  return $ Not @Shoot

shoot :: MonadIO m => ClSFS m cl () ()
shoot = constMCl _shoot
