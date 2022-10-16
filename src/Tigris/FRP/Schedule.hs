{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Tigris.FRP.Schedule where

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World

--base
import Control.Concurrent

-- transformers
import Control.Monad.Trans.Class


concurrentlySystem
  :: ( Clock (SystemT World IO) cl1
     , Clock (SystemT World IO) cl2
     , Time cl1 ~ Time cl2
     )
  => World
  -> Schedule (SystemT World IO) cl1 cl2
concurrentlySystem world = Schedule $ \cl1 cl2 -> do
  iMVar <- lift newEmptyMVar
  mvar  <- lift newEmptyMVar
  _ <- launchSubthread cl1 Left  iMVar mvar world
  _ <- launchSubthread cl2 Right iMVar mvar world
  initTime <- lift $ takeMVar iMVar
  _        <- lift $ takeMVar iMVar
  return (constM $ lift $ takeMVar mvar, initTime)
  where
    launchSubthread cl leftright iMVar mvar world = lift $ forkIO $ do
      (runningClock, initTime) <- runWith world $ initClock cl
      putMVar iMVar (initTime)
      runWith world $ reactimate $ runningClock >>> second (arr leftright) >>> arrM (lift . putMVar mvar)
