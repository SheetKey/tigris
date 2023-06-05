{-# LANGUAGE RecordWildCards #-}

module Tigris.Collision.DynamicAABBTree
  ( initDAABBTree
  , insertObject
  , removeObject
  , updateObject
  ) where

-- tigris
import Tigris.Collision.AABB

-- linear
import Linear hiding (rotate)

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- containers
import Data.IntMap.Strict as IM

-- base
import Control.Monad.ST
import Data.STRef
import Control.Monad (when)

data Node v a = Node 
  { aabb            :: AABB v a 
  , parentNodeIndex :: Int
  , nextNodeIndex   :: Int
  , leftNodeIndex   :: Int
  , rightNodeIndex  :: Int
  , height          :: Int
  , object          :: Int
  , isLeaf          :: Bool
  }

data DAABBTree v a = DAABBTree
  { rootNodeIndex     :: Int
  , nodes             :: V.Vector (Node v a)
  , nodeCount         :: Int
  , nodeCapacity      :: Int
  , growthSize        :: Int
  , nextFreeNodeIndex :: Int
  , aabbFatExtension  :: a
  }

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

nullNodes :: BB v a => Int -> V.Vector (Node v a)
nullNodes nodeCapacity = V.generate nodeCapacity $ \i -> if i /= nodeCapacity - 1
  then nullTreeNode { nextNodeIndex = i + 1 }
  else nullTreeNode

growNodes :: BB v a => Int -> Int -> Int -> V.Vector (Node v a) -> V.Vector (Node v a)
growNodes nodeCount nodeCapacity growthSize nodes = runST $ do
  nodesM <- (flip MV.unsafeGrow) growthSize =<< V.thaw nodes
  MV.iforM_ nodesM $ \i node -> if i < nodeCount
    then return ()
    else MV.unsafeWrite nodesM i $ nullTreeNode { nextNodeIndex = i + 1 }
  MV.unsafeWrite nodesM (nodeCapacity - 1) nullTreeNode
  V.unsafeFreeze nodesM

initDAABBTree :: BB v a => Int -> Int -> a -> DAABBTree v a
initDAABBTree nodeCapacity growthSize aabbFatExtension = DAABBTree
  { rootNodeIndex     = 0
  , nodeCount         = 0
  , nodeCapacity      = nodeCapacity
  , growthSize        = growthSize
  , nextFreeNodeIndex = 0
  , nodes             = nullNodes nodeCapacity
  , aabbFatExtension  = aabbFatExtension
  }

assert :: Bool -> String -> a -> a
assert tf str a = if tf then a else error str

allocateNode :: BB v a => DAABBTree v a -> (Int, DAABBTree v a)
allocateNode daabbTree = if nextFreeNodeIndex daabbTree == nullNode
  -- 'nodes' is full and needs expanding
  then assert (nodeCount daabbTree == nodeCapacity daabbTree)
       "'nodeCount /= nodeCapacity' when grwoing 'nodes'." $ 
       let nodes' = growNodes
                    (nodeCount daabbTree)
                    (nodeCapacity daabbTree)
                    (growthSize daabbTree)
                    (nodes daabbTree)
       in peel $ daabbTree { nodes = nodes', nextFreeNodeIndex = nodeCount daabbTree }
  else peel daabbTree
  where
    peel daabbTree = let nodeIndex = nextFreeNodeIndex daabbTree
                         node = nodes daabbTree V.! nodeIndex
                         node' = node
                           { object = nullNode
                           , parentNodeIndex = nullNode
                           , leftNodeIndex = nullNode
                           , rightNodeIndex = nullNode
                           , height = 0
                           , isLeaf = False
                           }
                         nodes' = runST $ do
                           nodesM <- V.thaw $ nodes daabbTree
                           MV.unsafeWrite nodesM nodeIndex node'
                           V.unsafeFreeze nodesM
      in ( nodeIndex
         , daabbTree
           { nextFreeNodeIndex = nextNodeIndex node
           , nodeCount = nodeCount daabbTree + 1
           , nodes = nodes'
           }
         )

freeNode :: Int -> DAABBTree v a -> DAABBTree v a
freeNode nodeIndex daabbTree =
  assert (0 <= nodeIndex && nodeIndex < nodeCapacity daabbTree)
  "'nodeIndex' outside of bounds." $
  assert (0 < nodeCount daabbTree)
  "'nodeCount' is not larger than 0." $
  let node = nodes daabbTree V.! nodeIndex
      node' = node { nextNodeIndex = nextFreeNodeIndex daabbTree, height = nullNode }
  in daabbTree
     { nodes = V.modify (\v -> MV.unsafeWrite v nodeIndex node') (nodes daabbTree)
     , nextFreeNodeIndex = nodeIndex
     , nodeCount = nodeCount daabbTree - 1
     }
    
insertObject :: BB v a => Int -> AABB v a -> DAABBTree v a -> (Int, DAABBTree v a)
insertObject objId objaabb daabbTree =
  let (objIndex, daabbTree') = allocateNode daabbTree
      objNode = nodes daabbTree' V.! objIndex
      objNode' = objNode
                 { aabb = fattenAABB (aabbFatExtension daabbTree') objaabb
                 , height = 0
                 , object = objId
                 , isLeaf = True }
      nodes' = V.modify (\v -> MV.unsafeWrite v objIndex objNode') (nodes daabbTree')
  in ( objIndex
     , insertLeaf objIndex (daabbTree' { nodes = nodes' })
     )

removeObject :: Int -> DAABBTree v a -> DAABBTree v a
removeObject objIndex daabbTree =
  assert (0 <= objIndex && objIndex < nodeCapacity daabbTree)
  "'objIndex' not within vector bounds." $
  assert (isLeaf $ nodes daabbTree V.! objIndex)
  "'objIndex' is not a leaf." $
  freeNode objIndex $ removeLeaf objIndex daabbTree

updateObject :: BB v a => Int -> AABB v a -> DAABBTree v a-> DAABBTree v a
updateObject objIndex objAABB daabbTree = 
  assert (0 <= objIndex && objIndex < nodeCapacity daabbTree)
  "'objIndex' outside vector bounds." $
  assert (isLeaf $ nodes daabbTree V.! objIndex)
  "'objIndex' is not a leaf." $
  if objNodeAABB `contains` objAABB
  then if hugeAABB `contains` objNodeAABB
       then daabbTree
       else reinsert
  else reinsert
  where
    r = aabbFatExtension daabbTree
    fatAABB = fattenAABB r objAABB
    hugeAABB = fattenAABB (4 * r) fatAABB
    objNode = nodes daabbTree V.! objIndex
    objNodeAABB = aabb objNode
    reinsert = let daabbTree' = removeLeaf objIndex daabbTree
                   newObjNode = objNode { aabb = fatAABB }
                   nodes' = V.modify
                            (\v -> MV.unsafeWrite v objIndex newObjNode)
                            (nodes daabbTree')
               in insertLeaf objIndex (daabbTree' { nodes = nodes' })

insertLeaf :: BB v a => Int -> DAABBTree v a-> DAABBTree v a
insertLeaf leafIndex daabbTree =
  if (rootNodeIndex daabbTree) == nullNode
  then daabbTree
       { rootNodeIndex = leafIndex
       , nodes = V.modify
                 (\v -> MV.unsafeModify v (\node -> node { parentNodeIndex = nullNode }) leafIndex)
                 (nodes daabbTree)
       }
  else daabbTreeFinal
  where
    whileM act = do
      b <- act
      when b $ whileM act
    tNodes = nodes daabbTree
    aabbL = aabb $ nodes daabbTree V.! leafIndex
    -- Stage 1: Find the best sibling.
    sibling = runST $ do
      indexM <- newSTRef $ rootNodeIndex daabbTree
      whileM $ do
        index <- readSTRef indexM
        let idxNode = tNodes V.! index
            idxAABB = aabb idxNode
            idxArea = area idxAABB
            child1 = leftNodeIndex  idxNode
            child2 = rightNodeIndex idxNode
            cost = area $ combine idxAABB aabbL
            inheritanceCost = cost - idxArea
            childCost child = let leafChildComb = area $ combine aabbL $ aabb $ tNodes V.! child
                              in if isLeaf $ tNodes V.! child
                                 then inheritanceCost + leafChildComb
                                 else (+) inheritanceCost $
                                      (-) leafChildComb $ area $ aabb $ tNodes V.! child
            cost1 = childCost child1
            cost2 = childCost child2
        if cost < cost1 && cost < cost2
          then return False
          else if cost1 < cost2
               then do { writeSTRef indexM child1; return True }
               else do { writeSTRef indexM child2; return True }
      readSTRef indexM
    -- Stage 2: Create a new parent
    siblingNode = tNodes V.! sibling
    oldParent = parentNodeIndex $ tNodes V.! sibling
    (newParent, daabbTree') = allocateNode daabbTree
    tNodes' = nodes daabbTree'
    newParentNode = (tNodes' V.! newParent)
                    { parentNodeIndex = oldParent
                    , aabb = combine aabbL (aabb siblingNode)
                    , height = height siblingNode + 1
                    }
    daabbTree'' = runST $ if oldParent /= nullNode
      then if leftNodeIndex (tNodes' V.! oldParent) == sibling
           then let oldParentNode = (tNodes' V.! oldParent) { leftNodeIndex = newParent }
                    newParentNode' = newParentNode
                                     { leftNodeIndex = sibling
                                     , rightNodeIndex = leafIndex
                                     }
                    siblingNode' = siblingNode { parentNodeIndex = newParent }
                    leafNode = (tNodes' V.! leafIndex) { parentNodeIndex = newParent }
                in
                  do
                    tNodesM' <- V.thaw tNodes'
                    MV.unsafeWrite tNodesM' oldParent oldParentNode
                    MV.unsafeWrite tNodesM' newParent newParentNode'
                    MV.unsafeWrite tNodesM' sibling siblingNode'
                    MV.unsafeWrite tNodesM' leafIndex leafNode
                    tNodes'' <- V.unsafeFreeze tNodesM'
                    return $ daabbTree' { nodes = tNodes'' }
           else let oldParentNode = (tNodes' V.! oldParent) { rightNodeIndex = newParent }
                    newParentNode' = newParentNode
                                     { leftNodeIndex = sibling
                                     , rightNodeIndex = leafIndex
                                     }
                    siblingNode' = siblingNode { parentNodeIndex = newParent }
                    leafNode = (tNodes' V.! leafIndex) { parentNodeIndex = newParent }
                in
                  do
                    tNodesM' <- V.thaw tNodes'
                    MV.unsafeWrite tNodesM' oldParent oldParentNode
                    MV.unsafeWrite tNodesM' newParent newParentNode'
                    MV.unsafeWrite tNodesM' sibling siblingNode'
                    MV.unsafeWrite tNodesM' leafIndex leafNode
                    tNodes'' <- V.unsafeFreeze tNodesM'
                    return $ daabbTree' { nodes = tNodes'' }
      else let newParentNode' = newParentNode
                                { leftNodeIndex = sibling
                                , rightNodeIndex = leafIndex
                                }
               siblingNode' = siblingNode { parentNodeIndex = newParent }
               leafNode = (tNodes' V.! leafIndex) { parentNodeIndex = newParent }
           in
             do
               tNodesM' <- V.thaw tNodes'
               MV.unsafeWrite tNodesM' newParent newParentNode'
               MV.unsafeWrite tNodesM' sibling siblingNode'
               MV.unsafeWrite tNodesM' leafIndex leafNode
               tNodes'' <- V.unsafeFreeze tNodesM'
               return $ daabbTree' { nodes = tNodes'', rootNodeIndex = newParent }
    -- Stage 3: walk bakc up the tree fixing heights and AABBs
    daabbTreeFinal = runST $ do
      indexM <- newSTRef oldParent
      nodesM <- V.thaw $ nodes daabbTree''
      whileM $ do
        idx <- readSTRef indexM
        idxNode <- MV.unsafeRead nodesM idx
        let child1 = leftNodeIndex idxNode
            child2 = rightNodeIndex idxNode
        assert (child1 /= nullNode)
          "'child1 == nullNode' in stage 3." $ 
          assert (child2 /= nullNode)
          "'child2 == nullNode' in stage 3." $ do
          child1Node <- MV.unsafeRead nodesM child1
          child2Node <- MV.unsafeRead nodesM child2
          MV.unsafeModify nodesM
            (\node -> node
                      { height = 1 + max (height child1Node) (height child2Node)
                      , aabb = combine (aabb child1Node) (aabb child2Node)
                      }
            )
            idx
          rotate idx nodesM
          writeSTRef indexM $ parentNodeIndex idxNode
          idx' <- readSTRef indexM
          return $ idx' /= nullNode
      nodes' <- V.unsafeFreeze nodesM
      return $ daabbTree'' { nodes = nodes' }

removeLeaf :: Int -> DAABBTree v a -> DAABBTree v a
removeLeaf leafIndex daabbTree = undefined

rotate :: BB v a => Int -> MV.MVector s (Node v a) -> ST s ()
rotate iA nodesM = do
  nodeA <- MV.unsafeRead nodesM iA
  if height nodeA < 2
    then return ()
    else do
    let iB = leftNodeIndex nodeA
        iC = rightNodeIndex nodeA
    nodeB <- MV.unsafeRead nodesM iB
    nodeC <- MV.unsafeRead nodesM iC
    case (height nodeB, height nodeC) of
      -- B is a leaf
      (0, hC) -> assert (hC > 0) "Both B and C are leafs but 'height A >2'." $ do
        let iF = leftNodeIndex nodeC
            iG = rightNodeIndex nodeC
        nodeF <- MV.unsafeRead nodesM iF
        nodeG <- MV.unsafeRead nodesM iG
        let baseCost = area $ aabb nodeC
            -- cost of swapping B and F
            aabbBG = combine (aabb nodeB) (aabb nodeG)
            costBF = area aabbBG
            -- cost of swapping B and G
            aabbBF = combine (aabb nodeB) (aabb nodeF)
            costBG = area aabbBF
        if baseCost < costBF && baseCost < costBG
          then return ()
          else if costBF < costBG
               then -- swap B and F
                 do
                   let nodeCHeight = 1 + max (height nodeB) (height nodeG)
                       nodeAHeight = 1 + max nodeCHeight (height nodeF)
                   MV.unsafeWrite nodesM iA $ nodeA
                     { leftNodeIndex = iF
                     , height = nodeAHeight
                     }
                   MV.unsafeWrite nodesM iC $ nodeC
                     { leftNodeIndex = iB
                     , aabb = aabbBG
                     , height = nodeCHeight
                     }
                   MV.unsafeWrite nodesM iB $ nodeB { parentNodeIndex = iC }
                   MV.unsafeWrite nodesM iF $ nodeF { parentNodeIndex = iA }
               else -- swap B and G
                 do
                   let nodeCHeight = 1 + max (height nodeB) (height nodeF)
                       nodeAHeight = 1 + max nodeCHeight (height nodeG)
                   MV.unsafeWrite nodesM iA $ nodeA
                     { leftNodeIndex = iG
                     , height = nodeAHeight
                     }
                   MV.unsafeWrite nodesM iC $ nodeC
                     { rightNodeIndex = iB
                     , aabb = aabbBF
                     , height = nodeCHeight
                     }
                   MV.unsafeWrite nodesM iB $ nodeB { parentNodeIndex = iC }
                   MV.unsafeWrite nodesM iG $ nodeG { parentNodeIndex = iA }
      -- C is a leaf
      (_, 0) -> do
        let iD = leftNodeIndex nodeB
            iE = rightNodeIndex nodeB
        nodeD <- MV.unsafeRead nodesM iD
        nodeE <- MV.unsafeRead nodesM iE
        let baseCost = area $ aabb nodeB
            -- cost of swapping C and D
            aabbCE = combine (aabb nodeC) (aabb nodeE)
            costCD = area aabbCE
            -- cost of swapping C and E
            aabbCD = combine (aabb nodeC) (aabb nodeD)
            costCE = area aabbCD
        if baseCost < costCD && baseCost < costCE
          then return ()
          else if costCD < costCE
               then -- swap C and D
                 do
                   let nodeBHeight = 1 + max (height nodeC) (height nodeE)
                       nodeAHeight = 1 + max nodeBHeight (height nodeD)
                   MV.unsafeWrite nodesM iA $ nodeA
                     { rightNodeIndex = iD
                     , height = nodeAHeight
                     }
                   MV.unsafeWrite nodesM iB $ nodeB
                     { leftNodeIndex = iC
                     , aabb = aabbCE
                     , height = nodeBHeight
                     }
                   MV.unsafeWrite nodesM iC $ nodeC { parentNodeIndex = iB }
                   MV.unsafeWrite nodesM iD $ nodeD { parentNodeIndex = iA }
               else -- swap C and E
                 do
                   let nodeBHeight = 1 + max (height nodeC) (height nodeD)
                       nodeAHeight = 1 + max nodeBHeight (height nodeE)
                   MV.unsafeWrite nodesM iA $ nodeA
                     { rightNodeIndex = iE
                     , height = nodeAHeight
                     }
                   MV.unsafeWrite nodesM iB $ nodeB
                     { rightNodeIndex = iC
                     , aabb = aabbCD
                     , height = nodeBHeight
                     }
                   MV.unsafeWrite nodesM iC $ nodeC { parentNodeIndex = iB }
                   MV.unsafeWrite nodesM iE $ nodeE { parentNodeIndex = iA }
      _      -> do
        let iD = leftNodeIndex nodeB
            iE = rightNodeIndex nodeB
            iF = leftNodeIndex nodeC
            iG = rightNodeIndex nodeC
        nodeD <- MV.unsafeRead nodesM iD
        nodeE <- MV.unsafeRead nodesM iE
        nodeF <- MV.unsafeRead nodesM iF
        nodeG <- MV.unsafeRead nodesM iG
        -- base cost
        let areaB = area $ aabb nodeB
            areaC = area $ aabb nodeC
            baseCost = areaB + areaC
        bestRotationM <- newSTRef RotateNone
        bestCostM <- newSTRef baseCost
        -- cost of swapping B and F
        let aabbBG = combine (aabb nodeB) (aabb nodeG)
            costBF = areaB + area aabbBG
        (\bestCost -> if costBF < bestCost
          then do
            writeSTRef bestRotationM RotateBF
            writeSTRef bestCostM costBF
          else return ()
          ) =<< readSTRef bestCostM
        -- cost of swapping B and G
        let aabbBF = combine (aabb nodeB) (aabb nodeF)
            costBG = areaB + area aabbBF
        (\bestCost -> if costBG < bestCost
          then do
            writeSTRef bestRotationM RotateBG
            writeSTRef bestCostM costBG
          else return ()
          ) =<< readSTRef bestCostM
        -- cost of swapping C and D
        let aabbCE = combine (aabb nodeC) (aabb nodeE)
            costCD = areaC + area aabbCE
        (\bestCost -> if costCD < bestCost
          then do
            writeSTRef bestRotationM RotateCD
            writeSTRef bestCostM costCD
          else return ()
          ) =<< readSTRef bestCostM
        -- cost of swapping C and E
        let aabbCD = combine (aabb nodeC) (aabb nodeD)
            costCE = areaC + area aabbCD
        (\bestCost -> if costCE < bestCost
          then do
            writeSTRef bestRotationM RotateCE
            writeSTRef bestCostM costCE
          else return ()
          ) =<< readSTRef bestCostM
        bestRotation <- readSTRef bestRotationM
        case bestRotation of
          RotateNone -> return ()
          RotateBF -> do
            let nodeCHeight = 1 + max (height nodeB) (height nodeG)
                nodeAHeight = 1 + max nodeCHeight (height nodeF)
            MV.unsafeWrite nodesM iA $ nodeA
              { leftNodeIndex = iF
              , height = nodeAHeight
              }
            MV.unsafeWrite nodesM iC $ nodeC
              { leftNodeIndex = iB
              , aabb = aabbBG
              , height = nodeCHeight
              }
            MV.unsafeWrite nodesM iB $ nodeB { parentNodeIndex = iC }
            MV.unsafeWrite nodesM iF $ nodeF { parentNodeIndex = iA }
          RotateBG -> do
            let nodeCHeight = 1 + max (height nodeB) (height nodeF)
                nodeAHeight = 1 + max nodeCHeight (height nodeG)
            MV.unsafeWrite nodesM iA $ nodeA
              { leftNodeIndex = iG
              , height = nodeAHeight
              }
            MV.unsafeWrite nodesM iC $ nodeC
              { rightNodeIndex = iB
              , aabb = aabbBF
              , height = nodeCHeight
              }
            MV.unsafeWrite nodesM iB $ nodeB { parentNodeIndex = iC }
            MV.unsafeWrite nodesM iF $ nodeG { parentNodeIndex = iA }
          RotateCD -> do
            let nodeBHeight = 1 + max (height nodeC) (height nodeE)
                nodeAHeight = 1 + max nodeBHeight (height nodeD)
            MV.unsafeWrite nodesM iA $ nodeA
              { rightNodeIndex = iD
              , height = nodeAHeight
              }
            MV.unsafeWrite nodesM iB $ nodeB
              { leftNodeIndex = iC
              , aabb = aabbCE
              , height = nodeBHeight
              }
            MV.unsafeWrite nodesM iC $ nodeC { parentNodeIndex = iB }
            MV.unsafeWrite nodesM iD $ nodeD { parentNodeIndex = iA }
          RotateCE -> do
            let nodeBHeight = 1 + max (height nodeC) (height nodeD)
                nodeAHeight = 1 + max nodeBHeight (height nodeE)
            MV.unsafeWrite nodesM iA $ nodeA
              { rightNodeIndex = iE
              , height = nodeAHeight
              }
            MV.unsafeWrite nodesM iB $ nodeB
              { rightNodeIndex = iC
              , aabb = aabbCD
              , height = nodeBHeight
              }
            MV.unsafeWrite nodesM iC $ nodeC { parentNodeIndex = iB }
            MV.unsafeWrite nodesM iE $ nodeE { parentNodeIndex = iA }
            

data TreeRotate = RotateNone
                | RotateBF
                | RotateBG
                | RotateCD
                | RotateCE
