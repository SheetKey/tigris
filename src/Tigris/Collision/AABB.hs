{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.Collision.AABB where

-- linear
import Linear hiding (rotate)

-- base
import Data.Kind (Type)

data AABB (v :: Type -> Type) a = AABB
  { lowerBound :: v a
  , upperBound :: v a
  }
  deriving (Eq, Show)

class (Ord a, Num a, Eq (v a)) => BB v a where
  nullAABB :: AABB v a
  fattenAABB :: a -> AABB v a -> AABB v a
  contains :: AABB v a -> AABB v a -> Bool
  area :: AABB v a -> a
  combine :: AABB v a -> AABB v a -> AABB v a
  overlaps :: AABB v a -> AABB v a -> Bool

instance (Ord a, Num a) => BB V3 a where
  nullAABB = AABB (V3 0 0 0) (V3 0 0 0)
  fattenAABB fat AABB {..} =
    AABB ((subtract fat) <$> lowerBound) ((+ fat) <$> upperBound)
  contains (AABB (V3 lx1 ly1 lz1) (V3 ux1 uy1 uz1)) (AABB (V3 lx2 ly2 lz2) (V3 ux2 uy2 uz2)) =
    lx1 <= lx2 
    && ly1 <= ly2
    && lz1 <= lz2
    && ux1 >= ux2
    && uy1 >= uy2
    && uz1 >= uz2
  area (AABB lowerBound upperBound) = 2 * (dx * dy + dy * dz + dz * dx)
    where (V3 dx dy dz) = upperBound - lowerBound
  combine
    (AABB (V3 lx1 ly1 lz1) (V3 ux1 uy1 uz1))
    (AABB (V3 lx2 ly2 lz2) (V3 ux2 uy2 uz2))
    = AABB
      (V3 (min lx1 lx2) (min ly1 ly2) (min lz1 lz2))
      (V3 (max ux1 ux2) (max uy1 uy2) (max uz1 uz2))
  overlaps
    (AABB (V3 lx1 ly1 lz1) (V3 ux1 uy1 uz1))
    (AABB (V3 lx2 ly2 lz2) (V3 ux2 uy2 uz2))
    = lx1 <= ux2
    && ux1 >= lx2
    && ly1 <= uy2
    && uy1 >= ly2
    && lz1 <= uz2
    && uz1 >= lz2

instance (Ord a, Num a) => BB V2 a where
  nullAABB = AABB (V2 0 0) (V2 0 0)
  fattenAABB fat AABB {..} =
    AABB ((subtract fat) <$> lowerBound) ((+ fat) <$> upperBound)
  contains (AABB (V2 lx1 ly1) (V2 ux1 uy1)) (AABB (V2 lx2 ly2) (V2 ux2 uy2)) =
    lx1 <= lx2 
    && ly1 <= ly2
    && ux1 >= ux2
    && uy1 >= uy2
  area AABB {..} = dx * dy
    where (V2 dx dy) = upperBound - lowerBound
  combine 
    (AABB (V2 lx1 ly1) (V2 ux1 uy1))
    (AABB (V2 lx2 ly2) (V2 ux2 uy2))
    = AABB
      (V2 (min lx1 lx2) (min ly1 ly2))
      (V2 (max ux1 ux2) (max uy1 uy2))
  overlaps
    (AABB (V2 lx1 ly1) (V2 ux1 uy1))
    (AABB (V2 lx2 ly2) (V2 ux2 uy2))
    = lx1 <= ux2
    && ux1 >= lx2
    && ly1 <= uy2
    && uy1 >= ly2
