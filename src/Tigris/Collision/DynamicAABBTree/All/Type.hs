module Tigris.Collision.DynamicAABBTree.All.Type where

-- tigris
import Tigris.Collision.AABB

-- vector
import qualified Data.Vector as V

-- containers
import qualified Data.IntMap.Strict as IM

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
  }
  deriving (Show)

nullNode :: Int
nullNode = -1

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
           }
