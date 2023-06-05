{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tigris.Collision.AABB where

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- linear
import Linear hiding (rotate)

data AABB (v :: * -> *) a = AABB
  { lowerBound :: v a
  , upperBound :: v a
  }
  deriving (Eq, Show)

class (Ord a, Num a) => BB v a where
  nullAABB :: AABB v a
  fattenAABB :: a -> AABB v a -> AABB v a
  contains :: AABB v a -> AABB v a -> Bool
  area :: AABB v a -> a
  combine :: AABB v a -> AABB v a -> AABB v a

instance (Ord a, Num a) => BB V3 a where
  nullAABB = AABB (V3 0 0 0) (V3 0 0 0)
  fattenAABB fat (AABB lowerBound upperBound) =
    AABB (((-) fat) <$> lowerBound) ((+ fat) <$> upperBound)
  contains (AABB (V3 lx1 ly1 lz1) (V3 ux1 uy1 uz1)) (AABB (V3 lx2 ly2 lz2) (V3 ux2 uy2 uz2)) =
    lx1 <= lx2 
    && ly1 <= ly2
    && lz1 <= lz2
    && ux1 >= ux2
    && uy1 >= uy2
    && uz1 >= uz2
  area (AABB lowerBound upperBound) = 2 * (dx * dy + dy * dz + dz * dx)
    where (V3 dx dy dz) = upperBound - lowerBound
  combine (AABB lb1 ub1) (AABB lb2 ub2) = AABB (min lb1 lb2) (max ub1 ub2)

instance (Ord a, Num a) => BB V2 a where
  nullAABB = AABB (V2 0 0) (V2 0 0)
  fattenAABB fat (AABB lowerBound upperBound) =
    AABB (((-) fat) <$> lowerBound) ((+ fat) <$> upperBound)
  contains (AABB (V2 lx1 ly1) (V2 ux1 uy1)) (AABB (V2 lx2 ly2) (V2 ux2 uy2)) =
    lx1 <= lx2 
    && ly1 <= ly2
    && ux1 >= ux2
    && uy1 >= uy2
  area (AABB lowerBound upperBound) = dx * dy
    where (V2 dx dy) = upperBound - lowerBound
  combine (AABB lb1 ub1) (AABB lb2 ub2) = AABB (min lb1 lb2) (max ub1 ub2)
  
