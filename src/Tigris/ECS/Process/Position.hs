{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Tigris.ECS.Process.Position where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.Graphics

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- sdl
import qualified SDL

nextPosition :: Rectangle CInt -> V2 Double -> Double -> Double -> Rectangle CInt
nextPosition p v s dT = modPntV (truncate . (*s) . (*dT) <$> v) p

normVelocity :: V2 CInt -> V2 Double
normVelocity = SDL.normalize . (fmap fromIntegral)

setX0 :: V2 CInt -> V2 CInt
setX0 (V2 _ y) = V2 0 y

setY0 :: V2 CInt -> V2 CInt
setY0 (V2 x _) = V2 x 0
  
-- old
-- _setPosition :: MonadIO m => Double -> SystemT' m ()
-- _setPosition dT = cmap $ \(Position p, NormVelocity v, Speed s) -> Position $ nextPosition p v s dT

_setPosition :: MonadIO m => Double -> SystemT' m ()
_setPosition dT = cmap $ \(Position (V4 _ n _ _), Velocity v, Speed s) ->
  Position $ V4
  n
  (nextPosition n (normVelocity v) s dT)
  (nextPosition n (normVelocity $ setY0 v) s dT)
  (nextPosition n (normVelocity $ setX0 v) s dT)

setPosition :: (MonadIO m, (Diff (Time cl)) ~ Double) => ClSFS m cl () ()
setPosition = sinceLastS >>> arrMCl _setPosition
