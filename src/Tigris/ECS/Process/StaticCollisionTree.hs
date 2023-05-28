module Tigris.ECS.Process.StaticCollisionTree where

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System
import Tigris.Collision.KDTreeMap

-- linear
import Linear

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

_staticCollisions :: MonadIO m => GL.GLfloat -> SystemT' m ()
_staticCollisions radius = do
  StaticCollisionTree kdtree <- get global
  cmap $ \(Position (V4 _ (V3 x _ z) _ _), HitStatic onHit collisions) ->
           let (_, collidedWith) = unzip $ inRadius radius (x, z) kdtree
           in HitStatic onHit $ collisions ++ collidedWith

staticCollisions :: MonadIO m => GL.GLfloat -> ClSFS m cl () ()
staticCollisions = constMCl . _staticCollisions
