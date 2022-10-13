{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Components where

-- mylib
import Tigris.Graphics

-- apecs
import Apecs
import Apecs.Stores

type All = ( Player
           , Position
           , Rotation
           , Destination
           , Camera
           , Velocity
           , Health
           , Image
           , ( SpriteSheet
             , RToMouse
             , BackgroundSize
             , SDLWindow
             , SDLRenderer
             )
           )

data Player = Player
instance Component Player where
  type Storage Player = Unique Player

-- This is the real position (destination rect).
-- The source rect is created from the sprite sheet.
newtype Position = Position (Rectangle CInt) 
instance Component Position where
  type Storage Position = Map Position

data Rotation = Rotation
  { angle :: CDouble
  , rotPntFrac :: (CInt, CInt) -- `(2, 2)` centers rotation.
  , flipXY :: V2 Bool
  }
instance Component Rotation where
  type Storage Rotation = Map Rotation

data RToMouse = RToMouse
instance Component RToMouse where
  type Storage RToMouse = Map RToMouse

-- Destination rect relative to camera.
newtype Destination = Destination (Rectangle CInt)
instance Component Destination where
  type Storage Destination = Map Destination

newtype Camera = Camera (Rectangle CInt)
instance Semigroup Camera where
  (<>) = error "Should not be used."
instance Monoid Camera where
  mempty = Camera $ mkRect 0 0 0 0
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

newtype BackgroundSize = BackgroundSize (V2 CInt)
instance Component BackgroundSize where
  type Storage BackgroundSize = ReadOnly (Unique BackgroundSize)

newtype SDLWindow = SDLWindow Window
instance Component SDLWindow where
  type Storage SDLWindow = ReadOnly (Unique SDLWindow)

newtype SDLRenderer = SDLRenderer Renderer
instance Component SDLRenderer where
  type Storage SDLRenderer = ReadOnly (Unique SDLRenderer)
