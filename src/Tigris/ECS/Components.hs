{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Components where

-- mylib
import Tigris.Graphics
import Tigris.ECS.Stores

-- base
import Data.Int

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
           , TextureC
           , ( SpriteSheet
             , RToMouse
             , WindowSize
             , SDLWindow
             , SDLRenderer
             , WindowResized
             , NormVelocity
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
instance Component Camera where
  type Storage Camera = TMVGlobal Camera

newtype Velocity = Velocity (V2 CInt) 
instance Component Velocity where
  type Storage Velocity = Map Velocity

newtype NormVelocity = NormVelocity (V2 Double)
instance Component NormVelocity where
  type Storage NormVelocity = Map NormVelocity

newtype Health = Health Integer 
instance Component Health where
  type Storage Health = Map Health
  
newtype TextureC = TextureC Texture
instance Component TextureC where
  type Storage TextureC = Map TextureC

data SpriteSheet = SpriteSheet
  { rowIndex :: CInt
  , colIndex :: CInt
  , maxColIndex :: CInt
  , frameWidth :: CInt
  , frameHeight :: CInt
  , waitTime :: Double
  , accTime :: Double
  }
instance Component SpriteSheet where
  type Storage SpriteSheet = Map SpriteSheet

-- rename to `WindowSize` and change storage type?
newtype WindowSize = WindowSize (V2 CInt)
instance Component WindowSize where
  type Storage WindowSize = ReadOnly (TMVGlobal WindowSize)

newtype SDLWindow = SDLWindow Window
instance Component SDLWindow where
  type Storage SDLWindow = ReadOnly (TMVGlobal SDLWindow)

newtype SDLRenderer = SDLRenderer Renderer
instance Component SDLRenderer where
  type Storage SDLRenderer = ReadOnly (TMVGlobal SDLRenderer)

newtype WindowResized = WindowResized (V2 Int32)
instance Component WindowResized where
  type Storage WindowResized = BTMVGlobal WindowResized
