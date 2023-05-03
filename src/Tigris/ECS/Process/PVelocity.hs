{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Process.PVelocity where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components

-- rhine
import FRP.Rhine hiding ((^+^))

-- apecs
import Apecs

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear

normVelocity :: (VEnum, VEnum) -> V3 GL.GLfloat
normVelocity (Z, Z)       = V3 0              0 0           
normVelocity (Z, One)     = V3 0              0 1           
normVelocity (Z, NOne)    = V3 0              0 (-1)        
normVelocity (One, Z)     = V3 1              0 0           
normVelocity (One, One)   = V3 ((sqrt 2)/2)   0 ((sqrt 2)/2)
normVelocity (One, NOne)  = V3 ((sqrt 2)/2)   0 (- (sqrt 2)/2)
normVelocity (NOne, Z)    = V3 (-1)           0 0           
normVelocity (NOne, One)  = V3 (- (sqrt 2)/2) 0 ((sqrt 2)/2)
normVelocity (NOne, NOne) = V3 (- (sqrt 2)/2) 0 (- (sqrt 2)/2)

_setPlayerVelocity :: MonadIO m => Double -> SystemT' m ()
_setPlayerVelocity dT = cmap $ \(PVelocity v) ->
  Velocity $ normVelocity v

setPlayerVelocity :: (MonadIO m, (Diff (Time cl)) ~ Double) => ClSFS m cl () ()
setPlayerVelocity = sinceLastS >>> arrMCl _setPlayerVelocity
