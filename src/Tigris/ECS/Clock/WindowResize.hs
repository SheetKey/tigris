{- |
A clock that ticks when the `Window` has
been resized.
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.ECS.Clock.WindowResize where

-- rhine
import FRP.Rhine hiding (get)

-- time
import Data.Time.Clock

-- apecs
import Apecs
import Apecs.Core

-- base
import Control.Concurrent

-- mylib
import Tigris.Graphics hiding (get)
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components

-- | This clock will tick when the `Window` has
--   been resized.
data WindowResizeClock = WindowResizeClock


instance MonadIO m => Clock (SystemT World m) WindowResizeClock where
  type Time WindowResizeClock = UTCTime
  type Tag  WindowResizeClock = V2 CInt

  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM $ do
          WindowResized size <- get global
          time <- liftIO getCurrentTime
          return (time, fromIntegral <$> size)
      , initialTime
      )

  -- NOTE: This version used Global store for event, not TMGlobal
  --initClock _ = do
  --  initialTime <- liftIO getCurrentTime
  --  return
  --    ( filterS $ constM $ do
  --        WindowResized mSize <- get global
  --        time <- liftIO getCurrentTime
  --        return $ (time, ) . (fmap fromIntegral) <$> mSize
  --    , initialTime
  --    )

  -- NOTE: Old version of `initClock` required `get` from SDL not Apecs
  --initClock _ = do
  --  initialTime <- liftIO getCurrentTime
  --  store :: Storage SDLWindow <- getStore
  --  SDLWindow win <- explGet store 0
  --  initWinSize <- get $ windowSize win
  --  mvar <- liftIO newEmptyMVar
  --  _ <- liftIO $ forkIO $ do
  --    reactimate $
  --      (feedback initWinSize $ arrM $ \((), lastSize) -> do
  --          size <- get $ windowSize win
  --          if size == lastSize
  --            then do return ((), size)
  --            else do currentTime <- getCurrentTime
  --                    putMVar mvar (currentTime, size)
  --                    return ((), size)
  --      )
  --  return ( constM $ liftIO $ takeMVar mvar, initialTime )
          
          
instance GetClockProxy WindowResizeClock

instance Semigroup WindowResizeClock where
  _ <> _ = WindowResizeClock
