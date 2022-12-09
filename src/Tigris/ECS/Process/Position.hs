{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Tigris.ECS.Process.Position where

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

nextPosition :: V3 GL.GLfloat -> V3 GL.GLfloat -> GL.GLfloat -> Double -> V3 GL.GLfloat
nextPosition p v s dT = p ^+^ ( v ^* (s * realToFrac dT) ) 

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

setX0 :: (VEnum, VEnum) -> (VEnum, VEnum)
setX0 (_, z) = (Z, z)

setZ0 :: (VEnum, VEnum) -> (VEnum, VEnum)
setZ0 (x, _) = (x, Z)
  
-- old
-- _setPosition :: MonadIO m => Double -> SystemT' m ()
-- _setPosition dT = cmap $ \(Position p, NormVelocity v, Speed s) -> Position $ nextPosition p v s dT

_setPosition :: MonadIO m => Double -> SystemT' m ()
_setPosition dT = cmap $ \(Position (V4 _ n _ _), Velocity v, Speed s) ->
  Position $ V4
  n
  (nextPosition n (normVelocity v) s dT)
  (nextPosition n (normVelocity $ setZ0 v) s dT)
  (nextPosition n (normVelocity $ setX0 v) s dT)

setPosition :: (MonadIO m, (Diff (Time cl)) ~ Double) => ClSFS m cl () ()
setPosition = sinceLastS >>> arrMCl _setPosition
