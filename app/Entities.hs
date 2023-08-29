module Entities where

-- tigris
import Tigris

-- vox-hs
import Vox.Tree
import Vox.Shape

-- base
import Control.Monad (forM)

-- random
import System.Random (randomRIO)

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- sdl2
import qualified SDL

-- apecs
import Apecs

-- vector
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

-- containers
import qualified Data.IntMap.Strict as M

-- linear
import Linear

player :: SystemT' IO Entity
player = 
  let p = V3 0 0 0
  in newEntity ( Player
                , Size (V4 (V3 (-16) 32 0) (V3 16 32 0) (V3 16 0 0) (V3 (-16) 0 0))
                , Position (V4 p p p p)
                , Speed 250
                , SpriteSheet 1 (4096-34) 0 0 (34 * 4) 34 34 1 2 0
                , PVelocity (Z, Z)
                , WantLeftClick 2
                , ( ProjStats 5 1 (Speed 300)
                  , ShootOffset ((V3 0 0 0), 16)
                  , HitStatic Stop []
                  , HitBox $ fattenAABB 16 nullAABB
                  )
                )
followPlayer :: Int -> SystemT' IO ()
followPlayer _id =
  newEntity_ ( Size (V4 (V3 (-16) 32 0) (V3 16 32 0) (V3 16 0 0) (V3 (-16) 0 0))
             , SpriteSheet 1 (4096) 0 0 (34 * 4) 34 34 1 2 0
             , Follows _id (V3 32 0 0)
             , Rotation 0 0 0 (2, 2) 
             , RToMouse
             )

staticPositionEntity :: SystemT' IO ()
staticPositionEntity =
  let p = V3 1000 0 (-1000)
  in newEntity_ ( Size (V4 (V3 (-8) 16 0) (V3 8 16 0) (V3 8 0 0) (V3 (-8) 0 0))
             , SpriteSheet 1 (4096) 0 0 (34 * 4) 34 34 1 2 0
             , Position (V4 p p p p)
             )

tree :: Position -> SystemT' IO ((GL.GLfloat, GL.GLfloat), Int)
tree pos@(Position (V4 _ (V3 x _ z) _ _)) = do
  Entity ety <- newEntity
                (Size (V4 (V3 (-115.5) 264 0) (V3 115.5 264 0) (V3 115.5 0 0) (V3 (-115.5) 0 0))
                , SpriteSheet 1 4096 1190 4096 1190 238 272 1 1000000000 0
                , pos
                , StaticCollider
                , HitBox $ fattenAABB 8 nullAABB
                )
  return ((x, z), ety)

trees :: SystemT' IO [((GL.GLfloat, GL.GLfloat), Int)]
trees = do
  tList <- liftIO $ nrandPositions 10
  forM tList tree 

setStaticCollisionTree :: [((GL.GLfloat, GL.GLfloat), Int)] -> SystemT' IO ()
setStaticCollisionTree collisionList = set global $ StaticCollisionTree $ fromList2 collisionList

nrandPositions :: Int -> IO [Position]
nrandPositions n = do
  randListX <- forM [1 .. n] $ \_ -> randomRIO (0, 1024)
  randListZ <- forM [1 .. n] $ \_ -> randomRIO (-1024, 0)
  let randList = zip randListX randListZ
      tList = (flip fmap) randList $ \(x, z) -> let p = V3 x 0 z in Position (V4 p p p p)
  return tList

wall :: Position -> SystemT' IO ((GL.GLfloat, GL.GLfloat), Int)
wall pos@(Position (V4 _ (V3 x _ z) _ _)) = do
  _ <- newEntity
                ( Size (V4 (V3 (-16) 32 16) (V3 16 32 16) (V3 16 0 16) (V3 (-16) 0 16))
                , SpriteSheet 1 (4096-68) 0 0 0 34 34 1 100 0
                , pos
                )
  Entity ety <- newEntity
                ( Size (V4 (V3 (-16) 32 16) (V3 16 32 16) (V3 16 32 (-16)) (V3 (-16) 32 (-16)))
                , SpriteSheet 1 (4096-68) 34 34 34 34 34 1 100 0
                , pos
                , StaticCollider
                , HitBox $ fattenAABB 16 nullAABB
                )
  return ((x, z), ety)

walls :: SystemT' IO [((GL.GLfloat, GL.GLfloat), Int)]
walls = do
  let xList = [64, 96 .. 256]
      zList = repeat (-64)
      list = zip xList zList
      wList = (flip fmap) list $ \(x, z) -> let p = V3 x 0 z in Position (V4 p p p p)
  forM wList wall

mkTree :: Parameters -> V3 GL.GLfloat-> SystemT' IO ()
mkTree par pos = do
  let tree = constructTree par 12345 False
      rTree = fromTree 5 tree
  _ <- newEntity (TreeModel rTree, Position (V4 pos pos pos pos))
  return ()
