{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Events where

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- containers
import Data.Sequence

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.Graphics

getEvents :: MonadIO m => ClSF m Busy () [Event]
getEvents = constM pollEvents

-- TODO: test performace of
getEvent :: MonadIO m => ClSF m cl () Event
getEvent = filterS $ constM pollEvent

eventBuffer :: Monad m => ResBuf m cl1 cl2 [a] (Maybe a)
eventBuffer = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ fromList a >< as
    amGet as   = case viewr as of
      EmptyR   -> return (Nothing, empty)
      as' :> a -> return (Just a , as'  )

_handleEventPayload :: MonadIO m => EventPayload -> SystemT' m ()
_handleEventPayload (WindowSizeChangedEvent e) = set global (WindowResized $ Just $ windowSizeChangedEventSize e)
_handleEventPayload _ = return ()

_handleMEvent :: MonadIO m => Maybe Event -> SystemT' m ()
_handleMEvent Nothing      = return ()
_handleMEvent (Just event) = _handleEventPayload $ eventPayload event
