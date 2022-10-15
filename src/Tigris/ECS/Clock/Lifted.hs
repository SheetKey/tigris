{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Tigris.ECS.Clock.Lifted where

-- rhine
import FRP.Rhine

-- mylib
import Tigris.ECS.World

-- apecs
import Apecs


data BusyS m = MonadIO m => BusyS (HoistClock IO (SystemT World m) Busy)

busyS :: MonadIO m => BusyS m
busyS = BusyS $ HoistClock Busy liftIO

instance MonadIO m => Clock (SystemT World m) (BusyS m) where
  type Time (BusyS m) = Time Busy
  type Tag  (BusyS m) = Tag Busy

  initClock (BusyS cl) = initClock cl

instance GetClockProxy (BusyS m)
