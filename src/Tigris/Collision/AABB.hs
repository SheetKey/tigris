module Tigris.Collision.AABB where


data AABB (v :: * -> *) = AABB
  { lowerBound :: v GL.GLfloat
  , upperBound :: v GL.GLfloat
  }
  deriving (Eq, Show)

class (forall a. Num a, Num (v a)) => Coordinate v where
  zero :: v a
  

nullAABB :: AABB
nullAABB = AABB (V3 0 0 0) (V3 0 0 0)

fattenAABB :: GL.GLfloat -> AABB -> AABB
fattenAABB fat (AABB lowerBound upperBound) =
  AABB (((-) fat) <$> lowerBound) ((+ fat) <$> upperBound)

contains :: AABB -> AABB -> Bool
contains
  (AABB (V3 lx1 ly1 lz1) (V3 ux1 uy1 uz1))
  (AABB (V3 lx2 ly2 lz2) (V3 ux2 uy2 uz2)) =
  lx1 <= lx2 
  && ly1 <= ly2
  && lz1 <= lz2
  && ux1 >= ux2
  && uy1 >= uy2
  && uz1 >= uz2

area :: AABB -> GL.GLfloat
area AABB {..} = 2 * (dx * dy + dy * dz + dz * dx)
  where (V3 dx dy dz) = upperBound - lowerBound

combine :: AABB -> AABB -> AABB
combine (AABB lb1 ub1) (AABB lb2 ub2) = AABB (min lb1 lb2) (max ub1 ub2)
