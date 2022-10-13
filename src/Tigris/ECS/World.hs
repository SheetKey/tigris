{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Tigris.ECS.World where

-- apecs
import Apecs

-- mylip
import Tigris.ECS.Components


makeWorld "World" [ ''Player
                  , ''Position
                  , ''Rotation
                  , ''Destination
                  , ''Camera
                  , ''Velocity
                  , ''Health
                  , ''Image
                  , ''SpriteSheet
                  , ''RToMouse
                  , ''BackgroundSize
                  , ''SDLWindow
                  , ''SDLRenderer
                  , ''WindowResized
                  ]
