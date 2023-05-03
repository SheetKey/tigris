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

-- linear
import Linear


sheetWidth :: Int
sheetWidth = 4096

sheetHeight :: Int
sheetHeight = 4096


-- | A collection of all components used as convenience
--   for deleting all of an entities components.
type All = ( Player
           , Rotation
           , PVelocity
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
newtype Size = Size (V4 (V3 GL.GLfloat))
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
--   Stores the previous, next, next in x, and next in z positions.
newtype Position = Position (V4 (V3 GL.GLfloat))
instance Component Position where
  type Storage Position = Map Position

-- | An entity can "follow" another entity.
--   For example, an equipped weapon will follow the player.
--   Has the id of the entity being followed and a
--   position offset.
data Follows = Follows Int (V3 GL.GLfloat)
instance Component Follows where
  type Storage Follows = Map Follows

data Plane = XY | YZ | XZ

-- | Entities may have a rotational component.
data Rotation = Rotation
  { xyangle :: GL.GLfloat    -- ^ The angle of rotation. 
  , yzangle :: GL.GLfloat    -- ^ The angle of rotation.
  , xzangle :: GL.GLfloat    -- ^ The angle of rotation.
  , rotPntFrac :: (Int, Int) -- ^ Values to be used to determine the center of rotation. `(2, 2)` centers rotation by setting the rotation point to `(V2 (w `div` 2) (h `div` 2))` where `w` and `h` are the width and height of the destination rectangle.
  }
instance Component Rotation where
  type Storage Rotation = Map Rotation

data RotationMat = RotationMat (M44 GL.GLfloat)
instance Component RotationMat where
  type Storage RotationMat = Map RotationMat

-- | Whether or not entities with a rotation
--   component should be rotated towards the mouse.
data RToMouse = RToMouse
instance Component RToMouse where
  type Storage RToMouse = Map RToMouse

data VEnum = Z | One | NOne deriving (Eq)

-- | The input velocity of the player.
--   This is necessary for properly handling player movement input keys.
--   note that this is the x,z velocity, as in opengl,
--   the y axis is vertical. The xz plane is horizontal
newtype PVelocity = PVelocity (VEnum, VEnum) 
instance Component PVelocity where
  type Storage PVelocity = Unique PVelocity

-- | The velocity of an entity. This quantity should be normed when it is set.
newtype Velocity = Velocity (V3 GL.GLfloat)
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

-- | OpenGL texture coordinates.
newtype UV = UV (V4 (V2 GL.GLfloat))
instance Component UV where
  type Storage UV = Map UV

-- | Determines what portion of the
--   `Texture` will be rendered.
data SpriteSheet = SpriteSheet
  { texId       :: Int -- ^ Texture uniform id.
  , rowIndex    :: Int -- ^ The current row index. Should be the top left coord of the current cell. (In pixels.)
  , colIndex    :: Int -- ^ The current col index. Should be the top left coord of the current cell. (In pixels.)
  , colMin      :: Int -- ^ This entity's min top left col.
  , colMax      :: Int -- ^ This entity's max top left col.
  , frameWidth  :: Int -- ^ The pixel width of a single frame.
  , frameHeight :: Int -- ^ The pixel height of a single frame.
  , borderWidth :: Int -- ^ The width of the border around the sprite in the texture atlas.
  , waitTime    :: Double -- ^ Difference in time to wait before changing frames.
  , accTime     :: Double  -- ^ An internal time accumulator that should be initialized as 0.
  }
instance Component SpriteSheet where
  type Storage SpriteSheet = Map SpriteSheet

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

-- | Holds global left click information.
newtype MouseLeftClick = MouseLeftClick (Maybe (SDL.InputMotion, V3 GL.GLfloat, V3 GL.GLfloat))
instance Semigroup MouseLeftClick where
  _ <> _ = error "Semigroup instance for 'MouseLeftClick' should not be used."
instance Monoid MouseLeftClick where
  mempty = MouseLeftClick Nothing
instance Component MouseLeftClick where
  type Storage MouseLeftClick = TQGlobal MouseLeftClick

-- | Entities may want access to the global leftclick values.
--   This type should hold two functions, one of type
--   'V3 GL.GLfloat -> SystemT' m Bool' and one of type
--   '(SDL.InputMotion, V3 GL.GLfloat) -> SystemT' m ()',
--   but this is not possible due to circular dependencies.
--   Thus two arrays should be created before starting the main loop
--   to hold these functions.
--   The functions are not unique, so index them by integers stored
--   in the 'WantLeftClick' type.
newtype WantLeftClick = WantLeftClick Int
instance Component WantLeftClick where
  type Storage WantLeftClick = Map WantLeftClick

-- | Set this tag when an entity should should a projectile.
-- holds the initial position of the projectile and
-- a point that it should be shot at.
newtype Shoot = Shoot (V3 GL.GLfloat, V3 GL.GLfloat)
instance Component Shoot where
  type Storage Shoot = Map Shoot

-- | An entity that can shoot a projectile must have this.
-- When creating the 'Shoot' component, add this offset to
-- the shooting entity's current position to determine the
-- initial position of the projective.
newtype ShootOffset = ShootOffset (V3 GL.GLfloat)
instance Component ShootOffset where
  type Storage ShootOffset = Map ShootOffset

-- | An entity holds the stats of the projectile it should spawn.
data ProjStats = ProjStats
  { damage :: Int
  , accuracy :: Int
  , speed :: Speed
  }
instance Component ProjStats where
  type Storage ProjStats = Map ProjStats
