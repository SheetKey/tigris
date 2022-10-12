{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Tigris.ECS.World where

-- apecs
import Apecs

-- mylip
import Tigris.ECS.Components


makeWorld "World" [ ''Position
                  , ''Velocity
                  ]
