{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tigris.ECS.Process.HitStatic where

-- rhine
import FRP.Rhine

-- apecs
import Apecs
import Apecs.Core

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.Collision.KDTreeMap

-- linear
import Linear

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- base
import Control.Monad (forM_)

hitBoxCollision :: V3 GL.GLfloat -> HitBox -> V3 GL.GLfloat -> HitBox -> Bool
hitBoxCollision (V3 x1 _ z1) (Rect l1 w1) (V3 x2 _ z2) (Rect l2 w2) =
  let minx1 = x1 - (w1 / 2)
      maxx1 = x1 + (w1 / 2)
      minz1 = z1 - (l1 / 2)
      maxz1 = z1 + (l1 / 2)
      minx2 = x2 - (w2 / 2)
      maxx2 = x2 + (w2 / 2)
      minz2 = z2 - (l2 / 2)
      maxz2 = z2 + (l2 / 2)
  in (minx1 <= maxx2) 
     && (maxx1 >= minx2)
     && (minz1 <= maxz2)
     && (maxz1 >= minz2)
hitBoxCollision (V3 x1 _ z1) (Rect l1 w1) (V3 x2 _ z2) (Circ r2)    =
  let minx1 = x1 - (w1 / 2)
      maxx1 = x1 + (w1 / 2)
      minz1 = z1 - (l1 / 2)
      maxz1 = z1 + (l1 / 2)
      x = max minx1 (min x2 maxx1)
      z = max minz1 (min z2 maxz1)
      dist = sqrt $
             (x - x2) * (x - x2)
             + (z - z2) * (z - z2)
  in dist < r2
hitBoxCollision (V3 x1 _ z1) (Circ r1) (V3 x2 _ z2) (Rect l2 w2) =
  let minx2 = x2 - (w2 / 2)
      maxx2 = x2 + (w2 / 2)
      minz2 = z2 - (l2 / 2)
      maxz2 = z2 + (l2 / 2)
      x = max minx2 (min x1 maxx2)
      z = max minz2 (min z1 maxz2)
      dist = sqrt $
             (x - x1) * (x - x1)
             + (z - z1) * (z - z1)
  in dist < r1
hitBoxCollision (V3 x1 _ z1) (Circ r1) (V3 x2 _ z2) (Circ r2) =
  let dist = sqrt $
             (x1 - x2) * (x1 - x2)
             + (z1 - z2) * (z1 - z2) 
  in dist < r1 + r2

_hitStatic :: MonadIO m => SystemT' m ()
_hitStatic = do
  s :: Storage (StaticCollider, Position, HitBox) <- getStore
  cmapM_ $ \( HitStatic onStaticCollision hits
            , Position (V4 last next nextX nextZ)
            , hb :: HitBox, entity :: Entity) -> do
    forM_ hits $ \ety -> do
      (_, Position (V4 _ pos _ _), shb :: HitBox) <- lift $ explGet s $ ety
      let hitEty = hitBoxCollision pos shb
      if hitEty next hb
        then case (onStaticCollision, hitEty nextX hb, hitEty nextZ hb) of
               (Stop, False, _) ->
                 set entity (HitStatic Stop [], Position $ V4 last nextX nextX nextZ)
               (Stop, _, False) ->
                 set entity (HitStatic Stop [], Position $ V4 last nextZ nextX nextZ)
               _                -> case onStaticCollision of
                                    Stop ->
                                      set entity (HitStatic Stop [], Position $ V4 last last nextX nextZ)
                                    Delete ->
                                      destroy entity (Proxy @All)
        else case onStaticCollision of
               Stop   -> set entity (HitStatic Stop [])
               Delete -> set entity (HitStatic Delete [])

hitStatic :: MonadIO m => ClSFS m cl () ()
hitStatic = constMCl _hitStatic
