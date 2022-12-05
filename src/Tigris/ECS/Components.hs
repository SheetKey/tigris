{- |
The component types and instances used in the ECS.
-}

{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Components where

-- mylib
import Tigris.ECS.Stores

-- base
import Data.Int

-- apecs
import Apecs
import Apecs.Stores

-- sdl
import qualified SDL

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- vector
import qualified Data.Vector.Storable as V

-- linear
import Linear


-- | A collection of all components used as convenience
--   for deleting all of an entities components.
type All = ( Player
           , Rotation
           , Velocity
           , Health
           , ( RToMouse
             , TileMapSize
             , Window
             , WindowResized
             , Speed
             , ( ColliderCell
               , Collisions
               )
             )
           )

-- | A unique component signalling that
--   the entity is the player.
data Player = Player
instance Component Player where
  type Storage Player = Unique Player

-- | The local space of an object. Centered at (0,0,0)
--   This will be transformed by model, view, and projection
--   matrices in order to be rendered.
newtype Size = Size (V.Vector (V3 GL.GLfloat))
instance Component Size where
  type Storage Size = Map Size

-- | The model matrix. Transforms the entity size to its
--   position in the world.
newtype Model = Model (M44 GL.GLfloat)
instance Component Model where
  type Storage Model = Map Model

-- | The view matrix. Functions as the "camera."
newtype View = View (M44 GL.GLfloat)
instance Component View where
  type Storage View = TMVGlobal View

-- | The projection matrix. 
newtype Projection = Projection (M44 GL.GLfloat)
instance Component Projection where
  type Storage Projection = TMVGlobal Projection

-- | The position of entities in the "World Space."
--   Used to create the model matrix for entities
--   with this component. 
--   Stores the previous, next, next in x, and next in y positions.
newtype Position = Position (V4 (V3 GL.GLfloat))
instance Component Position where
  type Storage Position = Map Position

-- | Entities may have a rotational component.
data Rotation = Rotation
  { angle :: Double           -- ^ The angle of rotation.
  , rotPntFrac :: (Int, Int) -- ^ Values to be used to determine the center of rotation. `(2, 2)` centers rotation by setting the rotation point to `(V2 (w `div` 2) (h `div` 2))` where `w` and `h` are the width and height of the destination rectangle.
  , flipXY :: V2 Bool          -- ^ Whether or not to flip in the x and y directions.
  }
instance Component Rotation where
  type Storage Rotation = Map Rotation

-- | Whether or not entities with a rotation
--   component should be rotated towards the mouse.
data RToMouse = RToMouse
instance Component RToMouse where
  type Storage RToMouse = Map RToMouse

-- -- | The destination rectangle, i.e., where an
-- --   entity will be rendered to the screen.
-- newtype Destination = Destination (Rectangle CInt)
-- instance Component Destination where
--   type Storage Destination = Map Destination
-- 
-- -- | Used to create the `Destination` rectangle
-- --   for entities with a position component.
-- newtype Camera = Camera (Rectangle CInt)
-- instance Component Camera where
--   type Storage Camera = TMVGlobal Camera

data VEnum = Z | One | NOne deriving (Eq)

-- | The velocity of an entity.
--   note that this is the x,z velocity, as in opengl,
--   the y axis is vertical. The xz plane is horizontal
newtype Velocity = Velocity (VEnum, VEnum) 
instance Component Velocity where
  type Storage Velocity = Map Velocity

-- | The speed of movement, multiplies the `NormVelocity`.
newtype Speed = Speed GL.GLfloat
instance Component Speed where
  type Storage Speed = Map Speed

-- | The health of an entity.
newtype Health = Health Integer 
instance Component Health where
  type Storage Health = Map Health

-- -- | The SDL `Texture` of an entity.
-- newtype Texture = Texture SDL.Texture
-- instance Component Texture where
--   type Storage Texture = Map Texture

-- -- | Determines what portion of the
-- --   `Texture` will be rendered.
-- data SpriteSheet = SpriteSheet
--   { rowIndex :: CInt    -- ^ Indexed starting at 0. Rows are different states, i.e., for a player they might include idle, walking, running, etc.
--   , colIndex :: CInt    -- ^ Indexed starting at 0. Columns are different frames of a single animation state.
--   , maxColIndex :: CInt -- ^ The maximum column index should be equal to the number of frames in the row minus 1.
--   , frameWidth :: CInt  -- ^ The pixel width of a single frame.
--   , frameHeight :: CInt -- ^ The pixel height of a single frame.
--   , waitTime :: Double  -- ^ Difference in time to wait before changing frames.
--   , accTime :: Double   -- ^ An internal time accumulator that should be initialized as 0.
--   }
-- instance Component SpriteSheet where
--   type Storage SpriteSheet = Map SpriteSheet

-- | The size of the tilemap in pixels. Used for bounding the `Camera`.
newtype TileMapSize = TileMapSize (V2 Int)
instance Component TileMapSize where
  type Storage TileMapSize = ReadOnly (TMVGlobal TileMapSize)

-- | The SDL `Window`.
newtype Window = Window SDL.Window
instance Component Window where
  type Storage Window = ReadOnly (TMVGlobal Window)

-- | OpenGL buffers and shader program
newtype GLBuffers = GLBuffers (GL.VertexArrayObject, GL.BufferObject, GL.BufferObject, GL.Program)
instance Component GLBuffers where
  type Storage GLBuffers = ReadOnly (TMVGlobal GLBuffers)

-- | Used for the `WindowResizedClock`. 
newtype WindowResized = WindowResized (V2 Int32)
instance Component WindowResized where
  type Storage WindowResized = BTMVGlobal WindowResized

-- | Use for bitwise comparison to determine if two entities might collide.
newtype ColliderCell = ColliderCell Int
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

