{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tigris.Collision.DynamicAABBTreeFixed
  ( AABB (..)
  , Node (..)
  , DAABBTree (..)
  , insertObject
  , removeObject
  , updateObject
  , initDAABBTree
  , emptyAABB
  , aabbUnion
  , aabbArea
  , computePairs
  ) where

-- linear
import Linear

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- fixed-vector
import qualified Data.Vector.Fixed.Boxed as V
import qualified Data.Vector.Fixed.Mutable as V
import qualified Data.Vector.Fixed.Cont as V hiding (generate, concat)
import qualified Data.Vector.Fixed as V (concat)

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
  deriving (Eq, Show)

data Node = Node
  { aabb            :: AABB
  , object          :: Int
  , parentNodeIndex :: Int
  , nextNodeIndex   :: Int
  , leftNodeIndex   :: Int
  , rightNodeIndex  :: Int
  , childrenCrossed :: Bool
  }
  deriving (Eq, Show)

data DAABBTree (nodeCapacity :: Nat) (growthSize :: Nat) = DAABBTree
  { objectNodeIndexMap :: IM.IntMap Int
  , nodes              :: V.Vec nodeCapacity Node
  , rootNodeIndex      :: Int
  , allocatedNodeCount :: Int
  , nextFreeNodeIndex  :: Int
  }

instance (V.Arity nodeCapacity) => Show (DAABBTree nodeCapacity growthSize) where
  show DAABBTree {..} = "objectNodeIndexMap: "
                        ++ show objectNodeIndexMap
                        ++ " nodes: "
                        ++ show nodes
                        ++ " rootNodeIndex: "
                        ++ show rootNodeIndex
                        ++ " allocatedNodeCount: "
                        ++ show allocatedNodeCount
                        ++ " nextFreeNodeIndex: "
                        ++ show nextFreeNodeIndex

type ColliderPairList = [(Int, Int)]

emptyAABB :: AABB
emptyAABB = AABB (V3 0 0 0) (V3 0 0 0)

isLeaf :: Node -> Bool
isLeaf Node {..} = leftNodeIndex == nullNode

aabbUnion :: AABB -> AABB -> AABB
aabbUnion (AABB lb1 ub1) (AABB lb2 ub2) = AABB (min lb1 lb2) (max ub1 ub2)

aabbArea :: AABB -> GL.GLfloat
aabbArea AABB {..} = 2 * (dx * dy + dy * dz + dz * dx)
  where (V3 dx dy dz) = upperBound - lowerBound

aabbCollides :: AABB -> AABB -> Bool
aabbCollides
  (AABB (V3 minX1 minY1 minZ1) (V3 maxX1 maxY1 maxZ1))
  (AABB (V3 minX2 minY2 minZ2) (V3 maxX2 maxY2 maxZ2)) =
  minX1 <= maxX2
  && maxX1 >= maxX2
  && minY1 <= maxY2
  && maxY1 >= minY2
  && minZ1 <= maxZ2
  && maxZ1 >= minZ2

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
    , childrenCrossed = False
    }
  let l = V.lengthM nodesM
  V.unsafeWrite nodesM (offset + l - 1) $ Node
    { aabb            = emptyAABB
    , object          = nullNode
    , parentNodeIndex = nullNode
    , nextNodeIndex   = nullNode
    , leftNodeIndex   = nullNode
    , rightNodeIndex  = nullNode
    , childrenCrossed = False
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
          allocatedNodeNextNodeIndex = nextNodeIndex $ (nodes daabbTree) `V.unsafeIndex` nodeIndex
      nodesM <- V.thaw $ nodes daabbTree
      V.unsafeWrite nodesM nodeIndex $ Node
        { aabb            = emptyAABB
        , object          = nullNode
        , parentNodeIndex = nullNode
        , nextNodeIndex   = allocatedNodeNextNodeIndex
        , leftNodeIndex   = nullNode
        , rightNodeIndex  = nullNode
        , childrenCrossed = False
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

deallocateNode
  :: ( PrimMonad m
     , KnownNat nodeCapacity
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano nodeCapacity)
     )
  => Int -> DAABBTree nodeCapacity growthSize -> m (DAABBTree nodeCapacity growthSize)
deallocateNode nodeIndex DAABBTree {..} = do
  nodesM <- V.thaw $ nodes
  V.unsafeWrite nodesM nodeIndex $ Node
    { aabb            = emptyAABB
    , object          = nullNode
    , parentNodeIndex = nullNode
    , nextNodeIndex   = nextFreeNodeIndex 
    , leftNodeIndex   = nullNode 
    , rightNodeIndex  = nullNode
    , childrenCrossed = False
    }
  nodes' <- V.freeze nodesM
  return $ DAABBTree
    { nodes = nodes'
    , nextFreeNodeIndex = nodeIndex
    , allocatedNodeCount = allocatedNodeCount - 1
    , ..
    }

fixUpwardsTree
  :: ( PrimMonad m
     , KnownNat nodeCapacity
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano nodeCapacity)
     )
  => Int -> DAABBTree nodeCapacity growthSize -> m (DAABBTree nodeCapacity growthSize)
fixUpwardsTree treeNodeIndex DAABBTree {..} = 
  if treeNodeIndex /= nullNode
  then let treeNode = nodes `V.unsafeIndex` treeNodeIndex
       in case (leftNodeIndex treeNode /= nullNode, rightNodeIndex treeNode /= nullNode) of
            (True, True) -> do
              let leftNode = nodes `V.unsafeIndex` leftNodeIndex treeNode
                  rightNode = nodes `V.unsafeIndex` rightNodeIndex treeNode
                  treeNodeParentNodeIndex = parentNodeIndex treeNode
              nodesM <- V.thaw nodes
              V.unsafeWrite nodesM treeNodeIndex $ Node
                { aabb            = aabbUnion (aabb leftNode) (aabb rightNode)
                , object          = object treeNode
                , parentNodeIndex = parentNodeIndex treeNode
                , nextNodeIndex   = treeNodeParentNodeIndex
                , leftNodeIndex   = leftNodeIndex treeNode
                , rightNodeIndex  = rightNodeIndex treeNode
                , childrenCrossed = False
                }
              nodes' <- V.freeze nodesM
              fixUpwardsTree (parentNodeIndex treeNode) $ DAABBTree { nodes = nodes', ..}
            _ -> error "'fixUpwardsTree' assertions failed."
  else return DAABBTree {..}

insertLeaf
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
  => Int
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize)
insertLeaf leafNodeIndex daabbTree =
  let leafNode = (nodes daabbTree) `V.unsafeIndex` leafNodeIndex
  in case ( parentNodeIndex leafNode == nullNode
          , leftNodeIndex leafNode == nullNode
          , rightNodeIndex leafNode == nullNode
          ) of 
       (True, True, True) ->
         if (rootNodeIndex daabbTree) == nullNode
         then return $ DAABBTree
              { objectNodeIndexMap = objectNodeIndexMap daabbTree
              , nodes              = nodes daabbTree
              , rootNodeIndex      = leafNodeIndex
              , allocatedNodeCount = allocatedNodeCount daabbTree
              , nextFreeNodeIndex  = nextFreeNodeIndex daabbTree
              }
         else do
           let treeNodeIndex = rootNodeIndex daabbTree
           findAndAttach leafNodeIndex treeNodeIndex daabbTree
       _ -> error "'insertLeaf' assertions failed."
  where
    findAndAttach leafNodeIndex treeNodeIndex daabbTree =
      let treeNode = nodes daabbTree `V.unsafeIndex` treeNodeIndex
      in if not (isLeaf treeNode)
      then 
        let leftNodeIndex' = leftNodeIndex treeNode
            rightNodeIndex' = rightNodeIndex treeNode
            leftNode = nodes daabbTree `V.unsafeIndex` leftNodeIndex'
            rightNode = nodes daabbTree `V.unsafeIndex` rightNodeIndex'
            leafNode = nodes daabbTree `V.unsafeIndex` leafNodeIndex
            leftNodeAABB = aabb leftNode
            rightNodeAABB = aabb rightNode
            leafNodeAABB = aabb leafNode
            combinedAABB = aabbUnion (aabb treeNode) (leafNodeAABB)
            combinedAABBSA = aabbArea combinedAABB
            treeNodeAABBSA = aabbArea $ aabb treeNode
            newParentNodeCost = 2 * combinedAABBSA
            minimumPushDownCost = 2 * (combinedAABBSA - treeNodeAABBSA)
            costLeft = let cost = minimumPushDownCost
                                  + aabbArea (aabbUnion leafNodeAABB leftNodeAABB)
                       in if isLeaf leftNode
                       then cost
                       else cost - aabbArea leftNodeAABB
            costRight = let cost = minimumPushDownCost
                                   + aabbArea (aabbUnion leafNodeAABB rightNodeAABB)
                        in if isLeaf rightNode
                           then cost
                           else cost - aabbArea rightNodeAABB
        in if newParentNodeCost < costLeft && newParentNodeCost < costRight
           then attachLeaf leafNodeIndex treeNodeIndex daabbTree
           else if costLeft < costRight
                then findAndAttach leafNodeIndex leftNodeIndex' daabbTree
                else findAndAttach leafNodeIndex rightNodeIndex' daabbTree
      else attachLeaf leafNodeIndex treeNodeIndex daabbTree
    attachLeaf leafNodeIndex leafSiblingIndex daabbTree = do
      let leafSiblingNode = (nodes daabbTree) `V.unsafeIndex` leafSiblingIndex
          oldParentIndex = parentNodeIndex leafSiblingNode
      leftright <- allocateNode daabbTree
      case leftright of
        Left  (newParentIndex, daabbTree') ->
          help leafNodeIndex leafSiblingIndex oldParentIndex newParentIndex daabbTree'
        Right (newParentIndex, daabbTree') -> error "'allocateNode' should never return 'Right' in 'attachLeaf'."
    help leafNodeIndex leafSiblingIndex oldParentIndex newParentIndex DAABBTree {..} = do
      let newParentNode = nodes `V.unsafeIndex` newParentIndex
          leafNode = nodes `V.unsafeIndex` leafNodeIndex
          leafSiblingNode = nodes `V.unsafeIndex` leafSiblingIndex
      nodesM <- V.thaw nodes
      V.unsafeWrite nodesM newParentIndex $ Node
        { aabb            = aabbUnion
                            (aabb $ nodes `V.unsafeIndex` leafNodeIndex)
                            (aabb $ nodes `V.unsafeIndex` leafSiblingIndex)
        , object          = object newParentNode
        , parentNodeIndex = oldParentIndex
        , nextNodeIndex   = nextNodeIndex newParentNode
        , leftNodeIndex   = leafSiblingIndex
        , rightNodeIndex  = leafNodeIndex
        , childrenCrossed = False
        }
      V.unsafeWrite nodesM leafNodeIndex $ Node
        { aabb            = aabb leafNode
        , object          = object leafNode
        , parentNodeIndex = newParentIndex
        , nextNodeIndex   = nextNodeIndex leafNode
        , leftNodeIndex   = leftNodeIndex leafNode
        , rightNodeIndex  = rightNodeIndex leafNode
        , childrenCrossed = False
        }
      V.unsafeWrite nodesM leafSiblingIndex $ Node
        { aabb            = aabb leafSiblingNode
        , object          = object leafSiblingNode
        , parentNodeIndex = newParentIndex
        , nextNodeIndex   = nextNodeIndex leafSiblingNode
        , leftNodeIndex   = leftNodeIndex leafSiblingNode
        , rightNodeIndex  = rightNodeIndex leafSiblingNode
        , childrenCrossed = False
        }
      if oldParentIndex == nullNode
        then
        do
          nodes' <- V.freeze nodesM
          return $ DAABBTree
            { nodes              = nodes'
            , rootNodeIndex      = newParentIndex
            , ..
            }
        else let oldParentNode = nodes `V.unsafeIndex` oldParentIndex
             in if (leftNodeIndex oldParentNode) == leafSiblingIndex
                then
                  do
                    V.unsafeWrite nodesM oldParentIndex $ Node
                      { aabb            = aabb oldParentNode
                      , object          = object oldParentNode
                      , parentNodeIndex = parentNodeIndex oldParentNode
                      , nextNodeIndex   = nextNodeIndex oldParentNode
                      , leftNodeIndex   = newParentIndex
                      , rightNodeIndex  = rightNodeIndex oldParentNode
                      , childrenCrossed = False
                      }
                    nodes' <- V.freeze nodesM
                    fixUpwardsTree newParentIndex $ DAABBTree
                      { nodes = nodes'
                      , ..
                      }
                else 
                  do
                    V.unsafeWrite nodesM oldParentIndex $ Node
                      { aabb            = aabb oldParentNode
                      , object          = object oldParentNode
                      , parentNodeIndex = parentNodeIndex oldParentNode
                      , nextNodeIndex   = nextNodeIndex oldParentNode
                      , leftNodeIndex   = leftNodeIndex oldParentNode
                      , rightNodeIndex  = newParentIndex
                      , childrenCrossed = False
                      }
                    nodes' <- V.freeze nodesM
                    fixUpwardsTree newParentIndex $ DAABBTree
                      { nodes = nodes'
                      , ..
                      }

removeLeaf
  :: ( PrimMonad m
     , KnownNat nodeCapacity
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano nodeCapacity)
     )
  => Int
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize)
removeLeaf leafNodeIndex daabbTree = 
  if leafNodeIndex == rootNodeIndex daabbTree
  then return $ daabbTree { rootNodeIndex = nullNode }
  else let leafNode = nodes daabbTree `V.unsafeIndex` leafNodeIndex
           leafParentNodeIndex = parentNodeIndex leafNode
           leafParentNode = nodes daabbTree `V.unsafeIndex` leafParentNodeIndex
           grandParentNodeIndex = parentNodeIndex leafParentNode
           siblingNodeIndex = if leftNodeIndex leafParentNode == leafNodeIndex
                              then rightNodeIndex leafParentNode
                              else leftNodeIndex leafParentNode
       in if siblingNodeIndex == nullNode
          then error "we must have a sibling"
          else let siblingNode = nodes daabbTree `V.unsafeIndex` siblingNodeIndex
               in if grandParentNodeIndex /= nullNode
                  then let grandParentNode = nodes daabbTree `V.unsafeIndex` grandParentNodeIndex
                       in if leftNodeIndex grandParentNode == leafParentNodeIndex 
                          then
                            do
                              nodesM <- V.thaw $ nodes daabbTree
                              V.unsafeWrite nodesM grandParentNodeIndex $
                                grandParentNode { leftNodeIndex = siblingNodeIndex }
                              V.unsafeWrite nodesM siblingNodeIndex $
                                siblingNode { parentNodeIndex = grandParentNodeIndex }
                              nodes' <- V.freeze nodesM
                              daabbTree' <- deallocateNode leafParentNodeIndex $
                                daabbTree { nodes = nodes' }
                              daabbTree'' <- fixUpwardsTree grandParentNodeIndex daabbTree'
                              let leafNode'' = nodes daabbTree'' `V.unsafeIndex` leafNodeIndex
                              nodesM'' <- V.thaw $ nodes daabbTree''
                              V.unsafeWrite nodesM'' leafNodeIndex $
                                leafNode'' { parentNodeIndex = nullNode }
                              nodes'' <- V.freeze nodesM''
                              return daabbTree'' { nodes = nodes'' }
                          else 
                            do
                              nodesM <- V.thaw $ nodes daabbTree
                              V.unsafeWrite nodesM grandParentNodeIndex $
                                grandParentNode { rightNodeIndex = siblingNodeIndex }
                              V.unsafeWrite nodesM siblingNodeIndex $
                                siblingNode { parentNodeIndex = grandParentNodeIndex }
                              nodes' <- V.freeze nodesM
                              daabbTree' <- deallocateNode leafParentNodeIndex $
                                daabbTree { nodes = nodes' }
                              daabbTree'' <- fixUpwardsTree grandParentNodeIndex daabbTree'
                              let leafNode'' = nodes daabbTree'' `V.unsafeIndex` leafNodeIndex
                              nodesM'' <- V.thaw $ nodes daabbTree''
                              V.unsafeWrite nodesM'' leafNodeIndex $
                                leafNode'' { parentNodeIndex = nullNode }
                              nodes'' <- V.freeze nodesM''
                              return daabbTree'' { nodes = nodes'' }
                  else
                    do
                      nodesM <- V.thaw $ nodes daabbTree
                      V.unsafeWrite nodesM siblingNodeIndex $
                        siblingNode { parentNodeIndex = nullNode }
                      nodes' <- V.freeze nodesM
                      daabbTree' <- deallocateNode leafParentNodeIndex $
                        daabbTree { nodes = nodes', rootNodeIndex = siblingNodeIndex }
                      let leafNode' = nodes daabbTree' `V.unsafeIndex` leafNodeIndex
                      nodesM' <- V.thaw $ nodes daabbTree'
                      V.unsafeWrite nodesM' leafNodeIndex $
                        leafNode' { parentNodeIndex = nullNode }
                      nodes'' <- V.freeze nodesM'
                      return daabbTree' { nodes = nodes'' }

updateLeaf
  :: ( PrimMonad m
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
  => Int
  -> AABB
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize)
updateLeaf leafNodeIndex newAABB daabbTree = do
  let leafNode = nodes daabbTree `V.unsafeIndex` leafNodeIndex
      node = leafNode { aabb = newAABB }
  daabbTree' <- removeLeaf leafNodeIndex daabbTree
  nodesM <- V.thaw $ nodes daabbTree'
  V.unsafeWrite nodesM leafNodeIndex node
  insertLeaf leafNodeIndex daabbTree'

insertObject
  :: ( PrimMonad m
     , KnownNat nodeCapacity
     , KnownNat growthSize
     , KnownNat (nodeCapacity + growthSize)
     , KnownNat (nodeCapacity + growthSize + growthSize)
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.Peano (growthSize + 1) ~ V.S (V.Peano growthSize)
     , V.ArityPeano (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano growthSize)
     , V.ArityPeano (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     , V.ArityPeano (V.Add
                      (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
                      (V.Peano growthSize))
     , V.ArityPeano (V.Add
                      (V.Add
                        (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
                        (V.Peano growthSize))
                      (V.Peano growthSize))
     , V.Peano (nodeCapacity + growthSize) ~ V.Add (V.Peano nodeCapacity) (V.Peano growthSize)
     , V.Peano ((nodeCapacity + growthSize) + 1)
       ~ V.S (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     , V.Peano ((nodeCapacity + growthSize) + growthSize)
       ~ V.Add (V.Add (V.Peano nodeCapacity) (V.Peano growthSize)) (V.Peano growthSize)
     , V.Peano (((nodeCapacity + growthSize) + growthSize) + 1)
       ~ V.S (V.Add (V.Add (V.Peano nodeCapacity) (V.Peano growthSize)) (V.Peano growthSize))
     )
  => Int
  -> AABB
  -> DAABBTree nodeCapacity growthSize
  -> m (Either
        (DAABBTree nodeCapacity growthSize)
        (DAABBTree (nodeCapacity + growthSize) growthSize))
insertObject objectId objectAABB daabbTree = do
  leftright <- allocateNode daabbTree
  case leftright of
    Left  (nodeIndex, daabbTree') -> Left  <$> help nodeIndex daabbTree'
    Right (nodeIndex, daabbTree') -> Right <$> help nodeIndex daabbTree'
  where
    help
      :: ( PrimMonad m
     , KnownNat nodeCapacity
     , KnownNat growthSize
     , KnownNat (nodeCapacity + growthSize)
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.Peano (growthSize + 1) ~ V.S (V.Peano growthSize)
     , V.ArityPeano (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano growthSize)
     , V.ArityPeano (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     , V.ArityPeano (V.Add
                      (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
                      (V.Peano growthSize))
     , V.Peano (nodeCapacity + growthSize) ~ V.Add (V.Peano nodeCapacity) (V.Peano growthSize)
     , V.Peano ((nodeCapacity + growthSize) + 1)
       ~ V.S (V.Add (V.Peano nodeCapacity) (V.Peano growthSize))
     )
      => Int
      -> DAABBTree nodeCapacity growthSize
      -> m (DAABBTree nodeCapacity growthSize)
    help nodeIndex daabbTree = do
      let node = nodes daabbTree `V.unsafeIndex` nodeIndex
          node' = node { aabb = objectAABB, object = objectId }
          objectNodeIndexMap' = IM.insert objectId nodeIndex $ objectNodeIndexMap daabbTree
      nodesM <- V.thaw $ nodes daabbTree
      V.unsafeWrite nodesM nodeIndex node'
      nodes' <- V.freeze nodesM
      insertLeaf nodeIndex $ daabbTree { nodes = nodes', objectNodeIndexMap = objectNodeIndexMap' }

removeObject
  :: ( PrimMonad m
     , KnownNat nodeCapacity
     , V.Peano (nodeCapacity + 1) ~ V.S (V.Peano nodeCapacity)
     , V.ArityPeano (V.Peano nodeCapacity)
     )
  => Int
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize)
removeObject objectId daabbTree = 
  case IM.lookup objectId (objectNodeIndexMap daabbTree) of
    Nothing        -> return daabbTree
    Just nodeIndex -> do
      daabbTree' <- deallocateNode nodeIndex =<< removeLeaf nodeIndex daabbTree
      let objectNodeIndexMap' = IM.delete nodeIndex (objectNodeIndexMap daabbTree')
      return $ daabbTree' { objectNodeIndexMap = objectNodeIndexMap' }

updateObject
  :: ( PrimMonad m
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
  => Int
  -> AABB
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize)
updateObject objectId newAABB daabbTree =
  case IM.lookup objectId (objectNodeIndexMap daabbTree) of
    Nothing        -> error "Object not in 'objectNodeIndexMap'."
    Just nodeIndex -> updateLeaf nodeIndex newAABB daabbTree

clearChildrenCrossFlagHelper
  :: ( PrimMonad m
     , V.Arity nodeCapacity
     )
  => Int
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize)
clearChildrenCrossFlagHelper nodeIndex DAABBTree {..} = do
  let node = nodes `V.unsafeIndex` nodeIndex
  nodesM <- V.thaw nodes
  V.unsafeWrite nodesM nodeIndex $ node { childrenCrossed = False }
  nodes' <- V.freeze nodesM
  if isLeaf node
    then return DAABBTree { nodes = nodes', .. }
    else do
    daabbTree' <- clearChildrenCrossFlagHelper (leftNodeIndex node)
                  DAABBTree { nodes = nodes', .. }
    clearChildrenCrossFlagHelper (rightNodeIndex node) DAABBTree { nodes = nodes', .. }

crossChildren
  :: ( PrimMonad m
     , V.Arity nodeCapacity
     )
  => Int
  -> ColliderPairList
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize, ColliderPairList)
crossChildren nodeIndex cpList daabbTree = 
  let node = nodes daabbTree `V.unsafeIndex` nodeIndex
  in if childrenCrossed node
     then return (daabbTree, cpList)
     else
       do
         (daabbTree', cpList') <- computePairsHelper
                                  (leftNodeIndex node) (rightNodeIndex node) cpList daabbTree
         nodesM <- V.thaw $ nodes daabbTree'
         V.unsafeWrite nodesM nodeIndex $ node { childrenCrossed = True }
         nodes' <- V.freeze nodesM
         return $ (daabbTree' { nodes = nodes' }, cpList')

computePairsHelper
  :: ( PrimMonad m
     , V.Arity nodeCapacity
     )
  => Int
  -> Int
  -> ColliderPairList
  -> DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize, ColliderPairList)
computePairsHelper nodeIndex1 nodeIndex2 cpList daabbTree =
  let node1 = nodes daabbTree `V.unsafeIndex` nodeIndex1
      node2 = nodes daabbTree `V.unsafeIndex` nodeIndex2
  in if isLeaf node1
     then if isLeaf node2
          then if aabbCollides (aabb node1) (aabb node1)
               then return (daabbTree, (nodeIndex1, nodeIndex2) : cpList)
               else return (daabbTree, cpList)
          else
            do
              (daabbTree', cpList') <- crossChildren nodeIndex2 cpList daabbTree
              (daabbTree'', cpList'') <- computePairsHelper
                                         nodeIndex1 (leftNodeIndex node2) cpList' daabbTree'
              computePairsHelper nodeIndex1 (rightNodeIndex node2) cpList'' daabbTree''
     else if isLeaf node2
          then
            do
              (daabbTree', cpList') <- crossChildren nodeIndex1 cpList daabbTree
              (daabbTree'', cpList'') <- computePairsHelper
                                         (leftNodeIndex node1) nodeIndex2 cpList' daabbTree'
              computePairsHelper (rightNodeIndex node1) nodeIndex2 cpList'' daabbTree''
          else
            do
              (daabbTree', cpList') <- crossChildren nodeIndex1 cpList daabbTree
              (daabbTree'', cpList'') <- crossChildren nodeIndex2 cpList' daabbTree'
              (daabbTree''', cpList''') <- computePairsHelper
                (leftNodeIndex node1) (leftNodeIndex node2) cpList'' daabbTree''
              (daabbTree'''', cpList'''') <- computePairsHelper
                (leftNodeIndex node1) (rightNodeIndex node2) cpList''' daabbTree'''
              (daabbTree''''', cpList''''') <- computePairsHelper
                (rightNodeIndex node1) (leftNodeIndex node2) cpList'''' daabbTree''''
              computePairsHelper
                (rightNodeIndex node1) (rightNodeIndex node2) cpList''''' daabbTree'''''

computePairs
  :: ( PrimMonad m
     , V.Arity nodeCapacity
     )
  => DAABBTree nodeCapacity growthSize
  -> m (DAABBTree nodeCapacity growthSize, ColliderPairList)
computePairs daabbTree =
  if rootNodeIndex daabbTree == nullNode
     || isLeaf (nodes daabbTree `V.unsafeIndex` rootNodeIndex daabbTree)
  then return (daabbTree, [])
  else
    do
      daabbTree' <- clearChildrenCrossFlagHelper (rootNodeIndex daabbTree) daabbTree
      let rootNode = nodes daabbTree' `V.unsafeIndex` rootNodeIndex daabbTree
      computePairsHelper (leftNodeIndex rootNode) (rightNodeIndex rootNode) [] daabbTree'
