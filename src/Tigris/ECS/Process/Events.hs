{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Events where


-- base
import Data.Int

-- rhine
import FRP.Rhine

-- apecs
import Apecs
import Apecs.System

-- containers
import Data.Sequence

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.FRP

-- sdl
import SDL ( Event (..)
           , EventPayload (..)
           , KeyboardEventData (..)
           , MouseButtonEventData (..)
           , MouseButton (..)
           , Keycode (..)
           , Keysym (..)
           , InputMotion (..)
           , Point (..)
           , V2 (..)
           , pattern KeycodeW
           , pattern KeycodeA
           , pattern KeycodeS
           , pattern KeycodeD
           , WindowSizeChangedEventData (..)
           , pollEvents
           , pollEvent
           )
  

getEvents :: MonadIO m => ClSFS m cl () [Event]
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
_handleKeycodePressed KeycodeW = cmap $ \(Player, Velocity (x,_)) -> Velocity (x, NOne)
_handleKeycodePressed KeycodeA = cmap $ \(Player, Velocity (_,z)) -> Velocity (NOne, z)
_handleKeycodePressed KeycodeS = cmap $ \(Player, Velocity (x,_)) -> Velocity (x, One)
_handleKeycodePressed KeycodeD = cmap $ \(Player, Velocity (_,z)) -> Velocity (One, z)
_handleKeycodePressed _ = return ()

_handleKeycodeReleased :: MonadIO m => Keycode -> SystemT' m ()
_handleKeycodeReleased KeycodeW = cmapIf (\(Player, Velocity (_,z)) -> z == NOne)
  $ \(Player, Velocity (x,_)) -> Velocity (x, Z)
_handleKeycodeReleased KeycodeA = cmapIf (\(Player, Velocity (x,_)) -> x == NOne)
  $ \(Player, Velocity (_,z)) -> Velocity (Z, z)
_handleKeycodeReleased KeycodeS = cmapIf (\(Player, Velocity (_,z)) -> z ==  One)
  $ \(Player, Velocity (x,_)) -> Velocity (x, Z)
_handleKeycodeReleased KeycodeD = cmapIf (\(Player, Velocity (x,_)) -> x ==  One)
  $ \(Player, Velocity (_,z)) -> Velocity (Z, z)
_handleKeycodeReleased _ = return ()

_handleKeyboardEvent :: MonadIO m => KeyboardEventData -> SystemT' m ()
--_handleKeyboardEvent (KeyboardEventData _ _ True _) = return ()
_handleKeyboardEvent (KeyboardEventData _ Pressed  _ keysym) = _handleKeycodePressed $ keysymKeycode keysym
_handleKeyboardEvent (KeyboardEventData _ Released _ keysym) = _handleKeycodeReleased $ keysymKeycode keysym

_handleMouseEvent :: MonadIO m => InputMotion -> MouseButton -> Point V2 Int32 -> SystemT' m ()
_handleMouseEvent = undefined

_handleMouseButtonEvent :: MonadIO m => MouseButtonEventData -> SystemT' m ()
_handleMouseButtonEvent (MouseButtonEventData _ pr _ lr _ pos) = _handleMouseEvent pr lr pos

_handleEventPayload :: MonadIO m => EventPayload -> SystemT' m ()
_handleEventPayload (WindowSizeChangedEvent e) = set global (WindowResized $ windowSizeChangedEventSize e)
_handleEventPayload (KeyboardEvent e) = _handleKeyboardEvent e
_handleEventPayload (MouseButtonEvent e) = _handleMouseButtonEvent e
_handleEventPayload _ = return ()

_handleMEvent :: MonadIO m => Maybe Event -> SystemT' m ()
_handleMEvent Nothing      = return ()
_handleMEvent (Just event) = _handleEventPayload $ eventPayload event

handleMEvent :: MonadIO m => ClSFS m (HoistClock IO (SystemT World m) Busy) (Maybe Event) ()
handleMEvent = arrMCl _handleMEvent

handleEvent :: World
            -> RhineS IO (SeqClockS IO
                          (HoistClock IO (SystemT World IO) Busy)
                          (HoistClock IO (SystemT World IO) Busy)
                        )
            () ()
handleEvent world = (getEvents @@ (HoistClock Busy liftIO))
              >-- eventBuffer -@- (concurrentlySystem world)
              --> (handleMEvent @@ (HoistClock Busy liftIO))

althandleEvent :: World
               -> RhineS IO (SeqClockS IO
                             (HoistClock IO (SystemT World IO) Busy)
                             (HoistClock IO (SystemT World IO) Busy)
                            )
               () ()
althandleEvent world = (getEvent @@ (HoistClock Busy liftIO))
                       >-- fifoUnbounded -@- (concurrentlySystem world)
                       --> (handleMEvent @@ (HoistClock Busy liftIO))

aalthandleEvent :: MonadIO m => ClSFS m cl () ()
aalthandleEvent = getEvents >>> arrMCl (\lst -> mapM_ (_handleMEvent . Just) lst)
