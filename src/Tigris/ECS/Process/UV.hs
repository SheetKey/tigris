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

mkUV :: Int -> Int -> Int -> Int -> Int -> UV
mkUV colIndex rowIndex frameWidth frameHeight borderWidth =
  let
    _tl = fmap fromIntegral $ V2 colIndex rowIndex
    _tr = fmap fromIntegral $ V2 (colIndex + frameWidth) rowIndex
    _br = fmap fromIntegral $ V2 (colIndex + frameWidth) (rowIndex - frameHeight)
    _bl = fmap fromIntegral $ V2 colIndex (rowIndex - frameHeight)
    w = fromIntegral sheetWidth
    h = fromIntegral sheetHeight
    b = fromIntegral borderWidth
    f = V2 (/ w) (/ h)
    g = V4
        (+ V2 (b / w) ((-b) / h))
        (+ V2 ((-b) / w) ((-b) / h))
        (+ V2 ((-b) / w) (b / h))
        (+ V2 (b / w) (b / h))
    tl = f <*> _tl
    tr = f <*> _tr
    br = f <*> _br
    bl = f <*> _bl
  in UV $ g <*> (V4 tl tr br bl)

spriteToUV :: SpriteSheet -> UV
spriteToUV SpriteSheet {..} = mkUV colIndex rowIndex frameWidth frameHeight borderWidth


_uv :: MonadIO m => SystemT' m ()
_uv = cmap spriteToUV

uv :: MonadIO m => ClSFS m cl () ()
uv = constMCl _uv
