{-# LANGUAGE RecordWildCards #-}

module Tigris.Collision.DynamicAABBTree.Type where

-- tigris
import Tigris.Collision.AABB

-- vector
import qualified Data.Vector as V

-- containers
import qualified Data.IntMap.Strict as IM

-- base
import Control.Monad.ST
import Data.STRef
import Control.Monad (when)
import Data.Functor.Identity

data Node v a = Node 
  { aabb            :: AABB v a 
  , parentNodeIndex :: Int
  , nextNodeIndex   :: Int
  , leftNodeIndex   :: Int
  , rightNodeIndex  :: Int
  , height          :: Int
  , object          :: Int
  , isLeaf          :: Bool
  , moved           :: Bool
  }
  deriving (Show)

data DAABBTree v a = DAABBTree
  { rootNodeIndex     :: Int
  , nodes             :: V.Vector (Node v a)
  , nodeCount         :: Int
  , nodeCapacity      :: Int
  , growthSize        :: Int
  , nextFreeNodeIndex :: Int
  , aabbFatExtension  :: a
  , objectIndexMap    :: IM.IntMap Int
  , movedBuffer       :: V.Vector Int
  }
  deriving (Show)

nullNode :: Int
nullNode = -1

assert :: Bool -> String -> a -> a
assert tf str a = if tf then a else error str

nullTreeNode :: BB v a => Node v a
nullTreeNode = Node
           { aabb            = nullAABB
           , parentNodeIndex = nullNode
           , nextNodeIndex   = nullNode
           , leftNodeIndex   = nullNode
           , rightNodeIndex  = nullNode
           , height          = nullNode
           , object          = nullNode
           , isLeaf          = False
           , moved           = False
           }

validateStructure :: Int -> DAABBTree v a -> ()
validateStructure idx DAABBTree {..} = go [idx] 
  where
    go [] = ()
    go (index:xs) = 
      if index == nullNode
      then ()
      else if index == rootNodeIndex 
           then assert (parentNodeIndex (nodes V.! index) == nullNode)
                "'index' is the root node but has non-null parent." $
                rest 
           else rest
      where
        rest = let node = nodes V.! index
                   child1 = leftNodeIndex node
                   child2 = rightNodeIndex node
               in if isLeaf node
                  then assert (child1 == nullNode)
                       "'node' is a leaf but has a non-null 'leftNodeIndex'." $
                       assert (child2 == nullNode)
                       "'node' is a leaf but has a non-null 'rightNodeIndex'." $
                       assert (height node == 0)
                       "'node' is a leaf but has non-zero 'height'." ()
                  else assert (0 <= child1 && child1 < nodeCapacity)
                       "'leftNodeIndex' is not valid." $
                       assert (0 <= child2 && child2 < nodeCapacity)
                       "'rightNodeIndex' is not valid." $
                       assert (parentNodeIndex (nodes V.! child1) == index)
                       "'leftNodeIndex' of 'index' does not have 'index' as its parent." $
                       assert (parentNodeIndex (nodes V.! child2) == index)
                       "'rightNodeIndex' of 'index' does not have 'index' as its parent." $
                       go (child1 : child2 : xs) 

validateMetrics :: BB v a => Int -> DAABBTree v a -> ()
validateMetrics idx DAABBTree {..} = go [idx]
  where
    go [] = ()
    go (index : xs) =
      if index == nullNode
      then ()
      else let node = nodes V.! index
               child1 = leftNodeIndex node
               child2 = rightNodeIndex node
           in if isLeaf node
              then ()
              else let height1 = height $ nodes V.! child1
                       height2 = height $ nodes V.! child2
                       heightNode = 1 + max height1 height2
                   in assert (height node == heightNode)
                      "Height of the node is not equal to one plus the max of its childrens' heights." $
                      let aabbNode = combine
                                     (aabb $ nodes V.! child1)
                                     (aabb $ nodes V.! child2)
                      in assert (aabb node == aabbNode)
                         "Node's aabb is not equal to the combination of its childrens' aabbs." $
                         go (child1 : child2 : xs) 

validate :: BB v a => DAABBTree v a -> ()
validate daabbTree = runIdentity $ do
  _ <- return $ validateStructure (rootNodeIndex daabbTree) daabbTree
  _ <- return $ validateMetrics (rootNodeIndex daabbTree) daabbTree
  return $ runST $ do
    if (nextFreeNodeIndex daabbTree) /= nullNode
      then do
      freeCountM <- newSTRef 0
      freeIndexM <- newSTRef (nextFreeNodeIndex daabbTree)
      whileM $ do
        freeIndex <- readSTRef freeIndexM
        assert (0 <= freeIndex && freeIndex < nodeCapacity daabbTree)
          "'freeIndex' is not valid." $ do
          let nextFreeIndex = nextNodeIndex $ nodes daabbTree V.! freeIndex
          writeSTRef freeIndexM nextFreeIndex
          modifySTRef freeCountM (+1)
          return $ nextFreeIndex /= nullNode
      freeCount <- readSTRef freeCountM
      assert (nodeCount daabbTree + freeCount == nodeCapacity daabbTree)
        "'nodeCount + freeCount /= nodeCapacity'." $
        return ()
      else assert (nodeCount daabbTree == nodeCapacity daabbTree)
           "No free nodes but 'nodeCount /= nodeCapacity'." $
           return ()
      where
        whileM act = do
          b <- act
          when b $ whileM act
