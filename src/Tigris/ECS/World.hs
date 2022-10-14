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
                  , ''Position
                  , ''Rotation
                  , ''Destination
                  , ''Camera
                  , ''Velocity
                  , ''Health
                  , ''TextureC
                  , ''SpriteSheet
                  , ''RToMouse
                  , ''TileMapSize
                  , ''SDLWindow
                  , ''SDLRenderer
                  , ''WindowResized
                  , ''NormVelocity
                  ]
