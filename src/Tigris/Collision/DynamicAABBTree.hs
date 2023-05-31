{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tigris.Collision.DynamicAABBTree where

-- linear
import Linear

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- fixed-vector
import qualified Data.Vector.Fixed.Boxed as V
import qualified Data.Vector.Fixed.Mutable as V
import qualified Data.Vector.Fixed.Cont as V hiding (generate, concat)
import qualified Data.Vector.Fixed as V hiding (generate)

-- containers
import Data.IntMap.Strict as IM

-- primitive
import Control.Monad.Primitive

-- base
import GHC.TypeLits
import Data.Proxy

data AABB = AABB
  { lowerBound :: V3 GL.GLfloat
  , upperBound :: V3 GL.GLfloat
  }

data Node = Node
  { aabb            :: AABB
  , object          :: Int
  , parentNodeIndex :: Int
  , nextNodeIndex   :: Int
  , leftNodeIndex   :: Int
  , rightNodeIndex  :: Int
  }

data DAABBTree (nodeCapacity :: Nat) (growthSize :: Nat) = DAABBTree
  { objectNodeIndexMap :: IM.IntMap Int
  , nodes              :: V.Vec nodeCapacity Node
  , rootNodeIndex      :: Int
  , allocatedNodeCount :: Int
  , nextFreeNodeIndex  :: Int
  }

emptyAABB :: AABB
emptyAABB = AABB (V3 0 0 0) (V3 0 0 0)

isLeaf :: Node -> Bool
isLeaf Node {..} = leftNodeIndex == nullNode

aabbUnion :: AABB -> AABB -> AABB
aabbUnion (AABB lb1 ub1) (AABB lb2 ub2) = AABB (min lb1 lb2) (max ub1 ub2)

aabbArea :: AABB -> GL.GLfloat
aabbArea AABB {..} = 2 * (dx * dy + dy * dz + dz * dx)
  where (V3 dx dy dz) = upperBound - lowerBound

nullNode :: Int
nullNode = -1

initNodes
  :: forall m nodeCapacity growthSize.
     ( PrimMonad m
     , KnownNat nodeCapacity
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano nodeCapacity)
     )
  => Int -> m (V.Vec nodeCapacity Node)
initNodes offset = do
  nodesM :: V.MVec nodeCapacity (PrimState m) Node <- V.generate $ \i -> Node
    { aabb            = emptyAABB
    , object          = nullNode
    , parentNodeIndex = nullNode
    , nextNodeIndex   = offset + i + 1
    , leftNodeIndex   = nullNode
    , rightNodeIndex  = nullNode
    }
  let l = V.lengthM nodesM
  V.unsafeWrite nodesM (offset + l - 1) $ Node
    { aabb            = emptyAABB
    , object          = nullNode
    , parentNodeIndex = nullNode
    , nextNodeIndex   = nullNode
    , leftNodeIndex   = nullNode
    , rightNodeIndex  = nullNode
    }
  V.freeze nodesM

_initDAABBTree
  :: forall m nodeCapacity growthSize.
     ( PrimMonad m
     , KnownNat nodeCapacity
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano nodeCapacity)
     )
  => Int -> m (DAABBTree nodeCapacity growthSize)
_initDAABBTree offset = do
  nodes <- initNodes offset
  return $ DAABBTree
    { objectNodeIndexMap = IM.empty
    , nodes              = nodes
    , rootNodeIndex      = nullNode
    , allocatedNodeCount = 0
    , nextFreeNodeIndex  = 0
    }

initDAABBTree
  :: forall m nodeCapacity growthSize.
     ( PrimMonad m
     , KnownNat nodeCapacity
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano nodeCapacity)
     )
  => m (DAABBTree nodeCapacity growthSize)
initDAABBTree = _initDAABBTree 0

resizeDAABBTree
  :: forall m nodeCapacity growthSize.
     ( PrimMonad m
     , KnownNat nodeCapacity
     , KnownNat growthSize
     , KnownNat (nodeCapacity + growthSize)
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.Peano (growthSize + 1) ~ V.S (V.Peano growthSize)
     , V.ArityPeano (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano growthSize)
     , V.ArityPeano (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     , V.Peano (nodeCapacity + growthSize) ~ V.Add (V.Peano nodeCapacity) (V.Peano growthSize)
     , V.Peano ((nodeCapacity + growthSize) + 1)
       ~ V.S (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     )
  => DAABBTree nodeCapacity growthSize
  -> m (DAABBTree (nodeCapacity + growthSize) growthSize)
resizeDAABBTree daabbTree = do
  newNodes :: V.Vec growthSize Node <- initNodes
    (fromIntegral $ natVal (Proxy :: Proxy nodeCapacity))
  return DAABBTree
    { objectNodeIndexMap = objectNodeIndexMap daabbTree
    , nodes              = (nodes daabbTree) `V.concat` newNodes
    , rootNodeIndex      = rootNodeIndex daabbTree
    , allocatedNodeCount = allocatedNodeCount daabbTree
    , nextFreeNodeIndex  = allocatedNodeCount daabbTree
    }

allocateNode
  :: forall m nodeCapacity growthSize.
     ( PrimMonad m
     , KnownNat nodeCapacity
     , KnownNat growthSize
     , KnownNat (nodeCapacity + growthSize)
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.Peano (growthSize + 1) ~ V.S (V.Peano growthSize)
     , V.ArityPeano (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano growthSize)
     , V.ArityPeano (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     , V.Peano (nodeCapacity + growthSize) ~ V.Add (V.Peano nodeCapacity) (V.Peano growthSize)
     , V.Peano ((nodeCapacity + growthSize) + 1)
       ~ V.S (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     )
  => DAABBTree nodeCapacity growthSize
  -> m (Either
        (Int, DAABBTree nodeCapacity growthSize)
        (Int, DAABBTree (nodeCapacity + growthSize) growthSize))
allocateNode daabbTree =
  if nextFreeNodeIndex daabbTree == nullNode
  then if toInteger (allocatedNodeCount daabbTree) == natVal (Proxy :: Proxy nodeCapacity)
       then
         do
           daabbTree' :: DAABBTree (nodeCapacity + growthSize) growthSize <-
             resizeDAABBTree daabbTree
           help Right daabbTree'
       else error "Assertion that 'allocatedNodeCount == nodeCapacity' failed in 'allocateNode'."
  else help Left daabbTree
  where
    help leftright daabbTree = do
      let nodeIndex = nextFreeNodeIndex daabbTree
          allocatedNodeNextNodeIndex = nextNodeIndex $ (nodes daabbTree) V.! nodeIndex
      nodesM <- V.thaw $ nodes daabbTree
      V.unsafeWrite nodesM nodeIndex $ Node
        { aabb            = emptyAABB
        , object          = nullNode
        , parentNodeIndex = nullNode
        , nextNodeIndex   = allocatedNodeNextNodeIndex
        , leftNodeIndex   = nullNode
        , rightNodeIndex  = nullNode
        }
      nodes' <- V.freeze nodesM
      return $ leftright
        (nodeIndex
        , DAABBTree
          { objectNodeIndexMap = objectNodeIndexMap daabbTree
          , nodes              = nodes'
          , rootNodeIndex      = rootNodeIndex daabbTree
          , allocatedNodeCount = allocatedNodeCount daabbTree + 1
          , nextFreeNodeIndex  = allocatedNodeNextNodeIndex
          }
        )
