{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Events where

-- rhine
import FRP.Rhine

-- apecs
import Apecs
import Apecs.System

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

_handleKeycodePressed :: MonadIO m => Keycode -> SystemT' m ()
_handleKeycodePressed KeycodeW = cmap $ \(Player, Velocity v) -> Velocity $ setYV2 (-1) v
_handleKeycodePressed KeycodeA = cmap $ \(Player, Velocity v) -> Velocity $ setXV2 (-1) v
_handleKeycodePressed KeycodeS = cmap $ \(Player, Velocity v) -> Velocity $ setYV2 1 v
_handleKeycodePressed KeycodeD = cmap $ \(Player, Velocity v) -> Velocity $ setXV2 1 v
_handleKeycodePressed _ = return ()

_handleKeycodeReleased :: MonadIO m => Keycode -> SystemT' m ()
_handleKeycodeReleased KeycodeW = cmapIf (\(Velocity v) -> getYV2 v == (-1))
  $ \(Player, Velocity v) -> Velocity $ setYV2 0 v
_handleKeycodeReleased KeycodeA = cmapIf (\(Velocity v) -> getXV2 v == (-1))
  $ \(Player, Velocity v) -> Velocity $ setXV2 0 v
_handleKeycodeReleased KeycodeS = cmapIf (\(Velocity v) -> getYV2 v ==   1 )
  $ \(Player, Velocity v) -> Velocity $ setYV2 0 v
_handleKeycodeReleased KeycodeD = cmapIf (\(Velocity v) -> getXV2 v ==   1 )
  $ \(Player, Velocity v) -> Velocity $ setXV2 0 v
_handleKeycodeReleased _ = return ()

_handleKeyboardEvent :: MonadIO m => KeyboardEventData -> SystemT' m ()
_handleKeyboardEvent (KeyboardEventData _ _ True _) = return ()
_handleKeyboardEvent (KeyboardEventData _ Pressed  _ keysym) = _handleKeycodePressed $ keysymKeycode keysym
_handleKeyboardEvent (KeyboardEventData _ Released _ keysym) = _handleKeycodeReleased $ keysymKeycode keysym

_handleEventPayload :: MonadIO m => EventPayload -> SystemT' m ()
_handleEventPayload (WindowSizeChangedEvent e)
  = set global (WindowResized $ windowSizeChangedEventSize e)
_handleEventPayload (KeyboardEvent e) = _handleKeyboardEvent e
_handleEventPayload _ = return ()

_handleMEvent :: MonadIO m => Maybe Event -> SystemT' m ()
_handleMEvent Nothing      = return ()
_handleMEvent (Just event) = _handleEventPayload $ eventPayload event
