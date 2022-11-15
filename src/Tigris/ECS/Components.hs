{- |
The component types and instances used in the ECS.
-}

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

-- sdl
import qualified SDL

-- | A collection of all components used as convenience
--   for deleting all of an entities components.
type All = ( Player
           , Position
           , Rotation
           , Destination
           , Camera
           , Velocity
           , Health
           , Texture
           , ( SpriteSheet
             , RToMouse
             , TileMapSize
             , Window
             , Renderer
             , WindowResized
             , NormVelocity
             , ( Speed
               , ColliderCell
               , Collisions
               )
             )
           )

-- | A unique component signalling that
--   the entity is the player.
data Player = Player
instance Component Player where
  type Storage Player = Unique Player

-- This is the real position (destination rect).
-- The source rect is created from the sprite sheet.
-- | The position of entities relative to
--   the tileset. 
newtype Position = Position (Rectangle CInt) 
instance Component Position where
  type Storage Position = Map Position

-- | Entities may have a rotational component.
data Rotation = Rotation
  { angle :: CDouble           -- ^ The angle of rotation.
  , rotPntFrac :: (CInt, CInt) -- ^ Values to be used to determine the center of rotation. `(2, 2)` centers rotation by setting the rotation point to `(V2 (w `div` 2) (h `div` 2))` where `w` and `h` are the width and height of the destination rectangle.
  , flipXY :: V2 Bool          -- ^ Whether or not to flip in the x and y directions.
  }
instance Component Rotation where
  type Storage Rotation = Map Rotation

-- | Whether or not entities with a rotation
--   component should be rotated towards the mouse.
data RToMouse = RToMouse
instance Component RToMouse where
  type Storage RToMouse = Map RToMouse

-- | The destination rectangle, i.e., where an
--   entity will be rendered to the screen.
newtype Destination = Destination (Rectangle CInt)
instance Component Destination where
  type Storage Destination = Map Destination

-- | Used to create the `Destination` rectangle
--   for entities with a position component.
newtype Camera = Camera (Rectangle CInt)
instance Component Camera where
  type Storage Camera = TMVGlobal Camera

-- | The velocity of an entity.
--   The x and y compnenets should only ever
--   be 0, 1, or -1.
newtype Velocity = Velocity (V2 CInt) 
instance Component Velocity where
  type Storage Velocity = Map Velocity

-- | The normalized velocity, used to change the
--   `Position` component.
newtype NormVelocity = NormVelocity (V2 Double)
instance Component NormVelocity where
  type Storage NormVelocity = Map NormVelocity

-- | The speed of movement, multiplies the `NormVelocity`.
newtype Speed = Speed Double
instance Component Speed where
  type Storage Speed = Map Speed

-- | The health of an entity.
newtype Health = Health Integer 
instance Component Health where
  type Storage Health = Map Health

-- | The SDL `Texture` of an entity.
newtype Texture = Texture SDL.Texture
instance Component Texture where
  type Storage Texture = Map Texture

-- | Determines what portion of the
--   `Texture` will be rendered.
data SpriteSheet = SpriteSheet
  { rowIndex :: CInt    -- ^ Indexed starting at 0. Rows are different states, i.e., for a player they might include idle, walking, running, etc.
  , colIndex :: CInt    -- ^ Indexed starting at 0. Columns are different frames of a single animation state.
  , maxColIndex :: CInt -- ^ The maximum column index should be equal to the number of frames in the row minus 1.
  , frameWidth :: CInt  -- ^ The pixel width of a single frame.
  , frameHeight :: CInt -- ^ The pixel height of a single frame.
  , waitTime :: Double  -- ^ Difference in time to wait before changing frames.
  , accTime :: Double   -- ^ An internal time accumulator that should be initialized as 0.
  }
instance Component SpriteSheet where
  type Storage SpriteSheet = Map SpriteSheet

-- | The size of the tilemap in pixels. Used for bounding the `Camera`.
newtype TileMapSize = TileMapSize (V2 CInt)
instance Component TileMapSize where
  type Storage TileMapSize = ReadOnly (TMVGlobal TileMapSize)

-- | The SDL `Window`.
newtype Window = Window SDL.Window
instance Component Window where
  type Storage Window = ReadOnly (TMVGlobal Window)

-- | The SDL `Renderer`.
newtype Renderer = Renderer SDL.Renderer
instance Component Renderer where
  type Storage Renderer = ReadOnly (TMVGlobal Renderer)

-- | Used for the `WindowResizedClock`. 
newtype WindowResized = WindowResized (V2 Int32)
instance Component WindowResized where
  type Storage WindowResized = BTMVGlobal WindowResized

-- | Use for bitwise comparison to determine if two entities might collide.
newtype ColliderCell = ColliderCell CInt
instance Component ColliderCell where
  type Storage ColliderCell = Map ColliderCell

-- TODO: potetntially have each entity store their own collisions;
-- the issue with that might be that the collision component should
-- get deleted after collisions are processed, so that only
-- entities with a collision are mapped overn with cmap.
-- If this is done concurrently, then the componenet might be deleted
-- while it is being written else where.
-- | Holds the current collisions.
newtype Collisions = Collisions (Int, Int)
instance Component Collisions where
  type Storage Collisions = BTQGlobal Collisions
