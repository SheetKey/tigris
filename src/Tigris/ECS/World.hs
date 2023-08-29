{- |
Template haskell to create certain necessary
typeclass instances and initialize stores.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Tigris.ECS.World where

-- apecs
import Apecs

-- mylip
import Tigris.ECS.Components


-- | Creates all things that are
--   necessary.
makeWorld "World" [ ''Player
                  , ''Rotation
                  , ''Position
                  , ''Model
                  , ''View
                  , ''Projection
                  , ''PVelocity
                  , ''Health
                  , ''RToMouse
                  , ''TileMapSize
                  , ''Window
                  , ''WindowResized
                  , ''Speed
                  , ''ColliderCell
                  , ''Collisions
                  , ''GLBuffers
                  , ''SpriteSheet
                  , ''UV
                  , ''Size
                  , ''Follows
                  , ''RotationMat
                  , ''MouseLeftClick
                  , ''WantLeftClick
                  , ''Shoot
                  , ''ProjStats
                  , ''Velocity
                  , ''ShootOffset
                  , ''StaticCollisionTree
                  , ''StaticCollider
                  , ''HitStatic
                  , ''HitBox
                  , ''TreeModel
                  , ''GLTreeBuffers
                  ]
