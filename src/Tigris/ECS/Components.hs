{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Components where

-- mylib
import Tigris.Graphics

-- apecs
import Apecs

type All = ( Position
           , Rotation
           , Destination
           , Camera
           , Velocity
           , Health
           , Image
           , SpriteSheet
           )

-- This is the real position (destination rect).
-- The source rect is created from the sprite sheet.
newtype Position = Position (Rectangle CInt) 
instance Component Position where
  type Storage Position = Map Position

data Rotation = Rotation
  { towardsMouse :: Bool
  , angle :: CDouble
  , rotPntFrac :: (CInt, CInt) -- uses `div` on destination rect
  , flipXY :: V2 Bool
  }
instance Component Rotation where
  type Storage Rotation = Map Rotation

-- Destination rect relative to camera.
newtype Destination = Destination (Rectangle CInt)
instance Component Destination where
  type Storage Destination = Map Destination

newtype Camera = Camera (Rectangle CInt)
instance Component Camera where
  type Storage Camera = Global Camera

newtype Velocity = Velocity (V2 CInt) 
instance Component Velocity where
  type Storage Velocity = Map Velocity

newtype Health = Health Integer 
instance Component Health where
  type Storage Health = Map Health
  
newtype Image = Image (IO Texture)
instance Component Image where
  type Storage Image = Map Image

data SpriteSheet = Sprite
  { index :: CInt
  , maxFrameIndex :: CInt
  , frameIndex :: CInt
  , width :: CInt
  , height :: CInt
  }
instance Component SpriteSheet where
  type Storage SpriteSheet = Map SpriteSheet

