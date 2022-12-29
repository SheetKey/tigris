{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Process.UV where

-- mylib
import Tigris.ECS.Components
import Tigris.ECS.System

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- linear
import Linear

sheetWidth :: Int
sheetWidth = 4096

sheetHeight :: Int
sheetHeight = 4096

_uv :: MonadIO m => SystemT' m ()
_uv = cmap $ \(SpriteSheet {..}) -> 
  let
    _tl = fmap fromIntegral $ V2 colIndex rowIndex
    _tr = fmap fromIntegral $ V2 (colIndex + frameWidth) rowIndex
    _br = fmap fromIntegral $ V2 (colIndex + frameWidth) (rowIndex - frameHeight)
    _bl = fmap fromIntegral $ V2 colIndex (rowIndex - frameHeight)
    f = V2 (/ fromIntegral sheetWidth) (/ fromIntegral sheetHeight)
    tl = f <*> _tl
    tr = f <*> _tr
    br = f <*> _br
    bl = f <*> _bl
  in UV $ V4 tl tr br bl

uv :: MonadIO m => ClSFS m cl () ()
uv = constMCl _uv
