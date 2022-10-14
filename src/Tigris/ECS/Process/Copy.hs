{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Copy where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.World
import Tigris.ECS.Components
import Tigris.Graphics hiding (get)
import Tigris.ECS.Clock

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs


_copy :: MonadIO m => SystemT' m ()
_copy = cmapM_ $ \(TextureC texture) -> do
  cmapM_ $ \(Destination dest, SpriteSheet {..}, Rotation {..}) -> do
    SDLRenderer ren <- get global
    copyEx ren
           texture
           (Just (mkRect (colIndex * frameWidth) (rowIndex * frameHeight) frameWidth frameHeight))
           (Just dest)
           angle
           ((\(x, y) (Rectangle _ (V2 w h)) -> Just (P (V2 (w `div` x) (h `div` y)))) rotPntFrac dest)
           flipXY

  cmapM_ $ \(Destination dest, SpriteSheet {..}, Not :: Not Rotation) -> do
    SDLRenderer ren <- get global
    copy ren
         texture
         (Just (mkRect (colIndex * frameWidth) (rowIndex * frameHeight) frameWidth frameHeight))
         (Just dest)

  cmapM_ $ \(Destination dest, Not :: Not SpriteSheet, Rotation {..}) -> do
    SDLRenderer ren <- get global
    copyEx ren
           texture
           Nothing
           (Just dest)
           angle
           ((\(x, y) (Rectangle _ (V2 w h)) -> Just (P (V2 (w `div` x) (h `div` y)))) rotPntFrac dest)
           flipXY

  cmapM_ $ \(Destination dest, Not :: Not SpriteSheet, Not :: Not Rotation) -> do
    SDLRenderer ren <- get global
    copy ren
         texture
         Nothing
         (Just dest)

  cmapM_ $ \(Position p, Not ::Not Destination, SpriteSheet {..}, Rotation {..}) -> do
    SDLRenderer ren <- get global
    copyEx ren
           texture
           (Just (mkRect (colIndex * frameWidth) (rowIndex * frameHeight) frameWidth frameHeight))
           (Just p)
           angle
           ((\(x, y) (Rectangle _ (V2 w h)) -> Just (P (V2 (w `div` x) (h `div` y)))) rotPntFrac p)
           flipXY

  cmapM_ $ \(Position p, Not ::Not Destination, SpriteSheet {..}, Not :: Not Rotation) -> do
    SDLRenderer ren <- get global
    copy ren
         texture
         (Just (mkRect (colIndex * frameWidth) (rowIndex * frameHeight) frameWidth frameHeight))
         (Just p)

  cmapM_ $ \(Position p, Not ::Not Destination, Not :: Not SpriteSheet, Rotation {..}) -> do
    SDLRenderer ren <- get global
    copyEx ren
           texture
           Nothing
           (Just p)
           angle
           ((\(x, y) (Rectangle _ (V2 w h)) -> Just (P (V2 (w `div` x) (h `div` y)))) rotPntFrac p)
           flipXY

  cmapM_ $ \(Position p, Not ::Not Destination, Not :: Not SpriteSheet, Not :: Not Rotation) -> do
    SDLRenderer ren <- get global
    copy ren
         texture
         Nothing
         (Just p)

copyAll :: MonadIO m => ClSFS m cl () ()
copyAll = constMCl _copy
