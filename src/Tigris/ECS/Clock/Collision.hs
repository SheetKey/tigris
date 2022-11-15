{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Tigris.ECS.Clock.Collision where

-- rhine
import FRP.Rhine hiding (get)

-- time
import Data.Time.Clock

-- apecs
import Apecs

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.World

data CollisionClock = CollisionClock

instance MonadIO m => Clock (SystemT World m) CollisionClock where
  type Time CollisionClock = UTCTime
  type Tag  CollisionClock = (Int, Int)

  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM $ do
          Collisions c <- get global
          time <- liftIO getCurrentTime
          return (time, c)
      , initialTime
      )

instance GetClockProxy CollisionClock

instance Semigroup CollisionClock where
  _ <> _ = CollisionClock
