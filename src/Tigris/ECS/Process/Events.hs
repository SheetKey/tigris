{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Events
  ( handleEvent
  , althandleEvent
  , aalthandleEvent
  , eventHandler
  ) where


-- base
import Data.Int

-- rhine
import FRP.Rhine

-- dunai
import Control.Monad.Trans.MSF.List (mapMSF)

-- apecs
import Apecs
import Apecs.System

-- containers
import Data.Sequence hiding (filter)

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.FRP
import Tigris.ECS.Process.MousePosition

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
_handleKeycodePressed KeycodeW = cmap $ \(Player, PVelocity (x,_)) -> PVelocity (x, NOne)
_handleKeycodePressed KeycodeA = cmap $ \(Player, PVelocity (_,z)) -> PVelocity (NOne, z)
_handleKeycodePressed KeycodeS = cmap $ \(Player, PVelocity (x,_)) -> PVelocity (x, One)
_handleKeycodePressed KeycodeD = cmap $ \(Player, PVelocity (_,z)) -> PVelocity (One, z)
_handleKeycodePressed _ = return ()

_handleKeycodeReleased :: MonadIO m => Keycode -> SystemT' m ()
_handleKeycodeReleased KeycodeW = cmapIf (\(Player, PVelocity (_,z)) -> z == NOne)
  $ \(Player, PVelocity (x,_)) -> PVelocity (x, Z)
_handleKeycodeReleased KeycodeA = cmapIf (\(Player, PVelocity (x,_)) -> x == NOne)
  $ \(Player, PVelocity (_,z)) -> PVelocity (Z, z)
_handleKeycodeReleased KeycodeS = cmapIf (\(Player, PVelocity (_,z)) -> z ==  One)
  $ \(Player, PVelocity (x,_)) -> PVelocity (x, Z)
_handleKeycodeReleased KeycodeD = cmapIf (\(Player, PVelocity (x,_)) -> x ==  One)
  $ \(Player, PVelocity (_,z)) -> PVelocity (Z, z)
_handleKeycodeReleased _ = return ()

_handleKeycode :: MonadIO m => (PVelocity -> Maybe PVelocity) -> SystemT' m ()
_handleKeycode f = cmap $ \(Player, oldv :: PVelocity) -> case f oldv of
  Just newv -> newv
  Nothing -> oldv

-- New type for keeping track of which keys are being held down.
data WalkKey = W | A | S | D deriving Eq

-- Using a clsf, I have access to 'feedback' providing internal state to recall which keys are currently held down.
-- This address movement concerns: If I hold w key and press and release s key, SDL does not report that w is still being held down. 
handleKeycodeP :: Monad m => ClSFS m cl (InputMotion, Keycode) (PVelocity -> Maybe PVelocity)
handleKeycodeP = feedback [] $ arr $ \((im, kcode), held) ->
  case im of
    Pressed  ->
      case kcode of
        KeycodeW -> if W `elem` held then (\(PVelocity (x,_)) -> Just $ PVelocity (x, NOne), held) else (\(PVelocity (x,_)) -> Just $ PVelocity (x, NOne), W : held)
        KeycodeA -> if A `elem` held then (\(PVelocity (_,z)) -> Just $ PVelocity (NOne, z), held) else (\(PVelocity (_,z)) -> Just $ PVelocity (NOne, z), A : held)
        KeycodeS -> if S `elem` held then (\(PVelocity (x,_)) -> Just $ PVelocity (x, One), held) else (\(PVelocity (x,_)) -> Just $ PVelocity (x, One), S : held)
        KeycodeD -> if D `elem` held then (\(PVelocity (_,z)) -> Just $ PVelocity (One, z), held) else (\(PVelocity (_,z)) -> Just $ PVelocity (One, z), D : held)
        _        -> (\_ -> Nothing, held)
    Released ->
      case kcode of
        KeycodeW -> (\(PVelocity (x,z)) -> 
                       if z == NOne
                       then if S `elem` held
                            then Just $ PVelocity (x, One)
                            else Just $ PVelocity (x, Z)
                       else Just $ PVelocity (x,z)
                    , filter (\a -> a /= W) held)
        KeycodeA -> (\(PVelocity (x,z)) ->
                       if x == NOne
                       then if D `elem` held
                            then Just $ PVelocity (One, z)
                            else Just $ PVelocity (Z, z)
                       else Just $ PVelocity (x,z)
                    , filter (\a -> a /= A) held)
        KeycodeS -> (\(PVelocity (x,z)) ->
                       if z == One
                       then if W `elem` held
                            then Just $ PVelocity (x, NOne)
                            else Just $ PVelocity (x, Z)
                       else Just $ PVelocity (x,z)
                    , filter (\a -> a /= S) held)
        KeycodeD -> (\(PVelocity (x,z)) ->
                       if x == One
                       then if A `elem` held
                            then Just $ PVelocity (NOne, z)
                            else Just $ PVelocity (Z, z)
                       else Just $ PVelocity (x,z)
                    , filter (\a -> a /= D) held)
        _        -> (\_ -> Nothing, held)

-- apply the calculated velocity function to the current velocity
applyKeycode :: MonadIO m => ClSFS m cl (PVelocity -> Maybe PVelocity) ()
applyKeycode = arrMCl _handleKeycode

-- combine clsfs
handleKeycode :: MonadIO m => ClSFS m cl (InputMotion, Keycode) ()
handleKeycode = handleKeycodeP >>> applyKeycode

-- format input for 'handlKeycode'
handleKeyboardEvent :: MonadIO m => ClSFS m cl KeyboardEventData ()
handleKeyboardEvent = arr (\(KeyboardEventData _ im _ keysym) -> (im, keysymKeycode keysym))
                      >>> handleKeycode

_handleKeyboardEvent :: MonadIO m => KeyboardEventData -> SystemT' m ()
--_handleKeyboardEvent (KeyboardEventData _ _ True _) = return ()
_handleKeyboardEvent (KeyboardEventData _ Pressed  _ keysym) = _handleKeycodePressed $ keysymKeycode keysym
_handleKeyboardEvent (KeyboardEventData _ Released _ keysym) = _handleKeycodeReleased $ keysymKeycode keysym

_handleMouseEvent :: MonadIO m => InputMotion -> MouseButton -> Point V2 Int32 -> SystemT' m ()
_handleMouseEvent motion ButtonLeft (P (V2 x y)) = do
  pos1 <- _inGameMousePos (x, y)
  --pos2 <- _planeInGameMousePos (x, y)
  pos2 <- _rayMousePos (x, y)
  set global $ MouseLeftClick $ Just (motion, pos1, pos2)
_handleMouseEvent _ _ _ = return ()

_handleMouseButtonEvent :: MonadIO m => MouseButtonEventData -> SystemT' m ()
_handleMouseButtonEvent (MouseButtonEventData _ pr _ lr _ pos) = _handleMouseEvent pr lr pos

-- needed since wasd handling is now a clsf
handleMouseButtonEvent :: MonadIO m => ClSFS m cl MouseButtonEventData ()
handleMouseButtonEvent = arrMCl _handleMouseButtonEvent

_handleEventPayload :: MonadIO m => EventPayload -> SystemT' m ()
_handleEventPayload (WindowSizeChangedEvent e) = set global (WindowResized $ windowSizeChangedEventSize e)
_handleEventPayload (KeyboardEvent e) = _handleKeyboardEvent e
_handleEventPayload (MouseButtonEvent e) = _handleMouseButtonEvent e
_handleEventPayload _ = return ()

-- needed since handling keyboard events now uses clsf feedback
handleEventPayload :: MonadIO m => ClSFS m cl EventPayload ()
handleEventPayload = proc payload -> do
  case payload of
    WindowSizeChangedEvent e ->
      (arrMCl $ \e -> set global (WindowResized $ windowSizeChangedEventSize e)) -< e
    KeyboardEvent e -> handleKeyboardEvent -< e
    MouseButtonEvent e -> handleMouseButtonEvent -< e
    _ -> returnA -< ()

-- needed since handling keyboard events now uses clsf feedback
mEventHandler :: MonadIO m => ClSFS m cl (Maybe Event) ()
mEventHandler = proc e -> do
  case e of
    Just event -> handleEventPayload -< eventPayload event
    Nothing -> returnA -< ()

eventHandler :: MonadIO m => ClSFS m cl () ()
eventHandler = getEvents
               >>> arr (\es -> eventPayload <$> es)
               >>> mapMSF handleEventPayload
               >>> arr (\_ -> ())

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
