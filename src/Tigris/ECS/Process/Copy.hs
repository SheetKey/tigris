{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.Copy where

-- mylib
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.Graphics

-- rhine
import FRP.Rhine hiding (get)

-- apecs
import Apecs

import SDL (copy, copyEx)


_copy :: MonadIO m => SystemT' m ()
_copy = do
  Renderer ren <- get global
  cmapM_ $ \(Texture texture, Destination dest, SpriteSheet {..}, Rotation {..}) -> do
    copyEx ren
      texture
      (Just (mkRect (colIndex * frameWidth) (rowIndex * frameHeight) frameWidth frameHeight))
      (Just dest)
      angle
      ((\(x, y) (Rectangle _ (V2 w h)) -> Just (P (V2 (w `div` x) (h `div` y)))) rotPntFrac dest)
      flipXY

  cmapM_ $ \(Texture texture, Destination dest, SpriteSheet {..}, Not :: Not Rotation) -> do
    copy ren
      texture
      (Just (mkRect (colIndex * frameWidth) (rowIndex * frameHeight) frameWidth frameHeight))
      (Just dest)

  cmapM_ $ \(Texture texture, Destination dest, Not :: Not SpriteSheet, Rotation {..}) -> do
    copyEx ren
      texture
      Nothing
      (Just dest)
      angle
      ((\(x, y) (Rectangle _ (V2 w h)) -> Just (P (V2 (w `div` x) (h `div` y)))) rotPntFrac dest)
      flipXY

  cmapM_ $ \(Texture texture, Destination dest, Not :: Not SpriteSheet, Not :: Not Rotation) -> do
    copy ren
      texture
      Nothing
      (Just dest)

copyAll :: MonadIO m => ClSFS m cl () ()
copyAll = constMCl _copy
