{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tigris.ECS.Process.HitStatic where

-- rhine
import FRP.Rhine hiding (next)

-- apecs
import Apecs
import Apecs.Core

-- tigris
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.Collision

-- linear
import Linear

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- base
import Data.Foldable (foldlM)

-- lens
import Control.Lens.Getter ((^.))

_hitStatic :: MonadIO m => SystemT' m ()
_hitStatic = do
  s :: Storage (StaticCollider, Position, HitBox) <- getStore
  cmapM_ $ \( HitStatic onStaticCollision hits
            , Position (V4 lastPos next nextX nextZ)
            , HitBox hb, entity :: Entity) ->
    case onStaticCollision of
      Stop -> do
        pos <- foldlM (\newNext ety -> do
                          (_, Position (V4 _ pos _ _), HitBox shb) <- lift $ explGet s $ ety
                          let hitEty = overlaps (translate shb (pos ^. _xz))
                          case ( hitEty $ translate hb $ next ^. _xz
                               , hitEty $ translate hb $ nextX ^. _xz
                               , hitEty $ translate hb $ nextZ ^. _xz) of
                            (False, _, _) -> return newNext
                            (True, False, _) -> if newNext == next || newNext == nextX
                                                then return nextX
                                                else return lastPos
                            (True, _, False) -> if newNext == next || newNext == nextZ
                                                then return nextZ
                                                else return lastPos
                            (True, True, True) -> return lastPos
                      ) next hits
        set entity (HitStatic Stop [], Position $ V4 lastPos pos nextX nextZ)
      Delete -> do
        shouldDelete <- foldlM (\tf ety -> do
                                   (_, Position (V4 _ pos _ _), HitBox shb) <- lift $ explGet s $ ety
                                   let hitEty = overlaps (translate shb (pos ^. _xz))
                                   if hitEty $ translate hb (next ^. _xz)
                                     then return True
                                     else return tf
                               ) False hits
        if shouldDelete
          then destroy entity (Proxy @All)
          else set entity (HitStatic Delete [])

hitStatic :: MonadIO m => ClSFS m cl () ()
hitStatic = constMCl _hitStatic
