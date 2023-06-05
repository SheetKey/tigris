{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Shoot where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- rhine
import FRP.Rhine hiding (normalize)

-- apecs
import Apecs

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

bullet :: MonadIO m => V3 GL.GLfloat -> Velocity -> ProjStats -> SystemT' m ()
bullet p v (ProjStats {..}) = newEntity_
  ( Size (V4 (V3 (-8) 0 (-8)) (V3 8 0 (-8)) (V3 8 0 8) (V3 (-8) 0 8))
  , Position (V4 p p p p)
  , SpriteSheet 1 (4096) 0 0 (34 * 4) 34 34 1 2 0
  , speed
  , v
  , HitStatic Delete []
  , Circ 8
  )

velFromTo :: (V3 GL.GLfloat, V3 GL.GLfloat) -> Velocity
velFromTo (from, to) = Velocity $ normalize $ to - from

_shoot :: MonadIO m => SystemT' m ()
_shoot = cmapM $ \(Shoot s@(pos, _), ps) -> do
  bullet pos (velFromTo s) ps
  return $ Not @Shoot

shoot :: MonadIO m => ClSFS m cl () ()
shoot = constMCl _shoot
