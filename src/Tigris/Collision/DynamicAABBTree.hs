{-# LANGUAGE RecordWildCards #-}

module Tigris.Collision.DynamicAABBTree
  ( initDAABBTree
  , insertObject
  , removeObject
  , updateObject
  , validate
  , DAABBTree
  ) where

-- tigris
import Tigris.Collision.AABB

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- containers
import Data.IntMap.Strict as IM

-- base
import Control.Monad.ST
import Data.STRef
import Control.Monad (when)
import Data.Functor.Identity
import Debug.Trace

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
  MV.unsafeWrite nodesM (nodeCapacity + growthSize - 1) nullTreeNode
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
  , objectIndexMap    = IM.empty
  }

assert :: Bool -> String -> a -> a
assert tf str a = if tf then a else error str

allocateNode :: BB v a => DAABBTree v a -> (Int, DAABBTree v a)
allocateNode daabbTree = if nextFreeNodeIndex daabbTree == nullNode
  -- 'nodes' is full and needs expanding
  then let nodes' = growNodes
                    (nodeCount daabbTree)
                    (nodeCapacity daabbTree)
                    (growthSize daabbTree)
                    (nodes daabbTree)
       in peel $ daabbTree 
          { nodes = nodes'
          , nextFreeNodeIndex = nodeCount daabbTree
          , nodeCapacity = nodeCapacity daabbTree + growthSize daabbTree
          }
  else peel daabbTree
  where
    peel daabbTree = let nodeIndex = nextFreeNodeIndex daabbTree
                         node = trace ("allocateNode, nodeIndex " ++ show nodeIndex) nodes daabbTree V.! nodeIndex
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
  let node = nodes daabbTree V.! nodeIndex
      node' = node { nextNodeIndex = nextFreeNodeIndex daabbTree, height = nullNode }
  in daabbTree
     { nodes = V.modify (\v -> MV.unsafeWrite v nodeIndex node') (nodes daabbTree)
     , nextFreeNodeIndex = nodeIndex
     , nodeCount = nodeCount daabbTree - 1
     }
    
insertObject :: BB v a => Int -> AABB v a -> DAABBTree v a -> DAABBTree v a
insertObject objId objaabb daabbTree =
  let (objIndex, daabbTree') = allocateNode daabbTree
      objNode = nodes daabbTree' V.! objIndex
      objNode' = objNode
                 { aabb = fattenAABB (aabbFatExtension daabbTree') objaabb
                 , height = 0
                 , object = objId
                 , isLeaf = True }
      nodes' = V.modify (\v -> MV.unsafeWrite v objIndex objNode') (nodes daabbTree')
  in insertLeaf objIndex $ daabbTree'
     { nodes = nodes'
     , objectIndexMap = IM.insert objId objIndex $ objectIndexMap daabbTree' 
     }

removeObject :: BB v a => Int -> DAABBTree v a -> DAABBTree v a
removeObject objId daabbTree =
  let objIndex = objectIndexMap daabbTree IM.! objId
  in freeNode objIndex $ removeLeaf objIndex $ daabbTree
     { objectIndexMap = IM.delete objId (objectIndexMap daabbTree) }

updateObject :: BB v a => Int -> AABB v a -> DAABBTree v a-> DAABBTree v a
updateObject objId objAABB daabbTree = 
  if objNodeAABB `contains` objAABB
  then if hugeAABB `contains` objNodeAABB
       then daabbTree
       else reinsert
  else reinsert
  where
    objIndex = objectIndexMap daabbTree IM.! objId
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
    aabbL = trace ("insertLeaf, leafIndex " ++ show leafIndex) aabb $ nodes daabbTree V.! leafIndex
    -- Stage 1: Find the best sibling.
    sibling = if isLeaf $ tNodes V.! rootNodeIndex daabbTree
              then rootNodeIndex daabbTree
              else runST $ do
      indexM <- newSTRef $ rootNodeIndex daabbTree
      whileM $ do
        index <- readSTRef indexM
        let idxNode = trace ("insertLeaf, index " ++ show index) tNodes V.! index
            idxAABB = aabb idxNode
            idxArea = area idxAABB
            child1 = leftNodeIndex  idxNode
            child2 = rightNodeIndex idxNode
            cost = area $ combine idxAABB aabbL
            inheritanceCost = cost - idxArea
            childCost child = let leafChildComb = area $ combine aabbL $ aabb $ tNodes V.! child
                              in if trace ("insertLeaf, child " ++ show child) isLeaf $ tNodes V.! child
                                 then inheritanceCost + leafChildComb
                                 else (+) inheritanceCost $
                                      (-) leafChildComb $ area $ aabb $ tNodes V.! child
            cost1 = childCost child1
            cost2 = childCost child2
        if cost < cost1 && cost < cost2
          then return False
          else if cost1 < cost2
               then do { writeSTRef indexM child1; return $ child1 /= nullNode }
               else do { writeSTRef indexM child2; return $ child2 /= nullNode }
      readSTRef indexM
    -- Stage 2: Create a new parent
    siblingNode = tNodes V.! sibling
    oldParent = trace ("insertLeaf, sibling " ++ show sibling) parentNodeIndex $ tNodes V.! sibling
    (newParent, daabbTree') = allocateNode daabbTree
    tNodes' = nodes daabbTree'
    newParentNode = trace ("insertLeaf, newParent " ++ show newParent) (tNodes' V.! newParent)
                    { parentNodeIndex = oldParent
                    , aabb = combine aabbL (aabb siblingNode)
                    , height = height siblingNode + 1
                    }
    daabbTree'' = runST $ if oldParent /= nullNode
      then if trace ("insertLeaf, oldParent " ++ show oldParent) leftNodeIndex (tNodes' V.! oldParent) == sibling
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

removeLeaf :: BB v a => Int -> DAABBTree v a -> DAABBTree v a
removeLeaf leafIndex daabbTree = if leafIndex == rootNodeIndex daabbTree
  then daabbTree
  else let parentIndex = parentNodeIndex $ nodes daabbTree V.! leafIndex
           parentNode = nodes daabbTree V.! parentIndex
           grandParentIndex = parentNodeIndex parentNode
           grandParentNode = nodes daabbTree V.! grandParentIndex
           siblingIndex = if leftNodeIndex parentNode == leafIndex
                          then rightNodeIndex parentNode
                          else leftNodeIndex parentNode
           siblingNode = nodes daabbTree V.! siblingIndex
       in if grandParentIndex /= nullNode
             -- destroy parent and connect sibling to grandparent
          then let grandParentNode' = if leftNodeIndex grandParentNode == parentIndex
                                      then grandParentNode { leftNodeIndex = siblingIndex }
                                      else grandParentNode { rightNodeIndex = siblingIndex }
                   siblingNode' = siblingNode { parentNodeIndex = grandParentIndex }
                   nodes' = runST $ do
                     nodesM <- V.thaw $ nodes daabbTree
                     MV.unsafeWrite nodesM grandParentIndex grandParentNode'
                     MV.unsafeWrite nodesM siblingIndex siblingNode'
                     V.unsafeFreeze nodesM
                   daabbTree' = freeNode parentIndex $ daabbTree { nodes = nodes' }
                     -- adjust ancestor bounds
                   nodes'' = runST $ do
                     idxM <- newSTRef grandParentIndex
                     nodesM <- V.thaw $ nodes daabbTree'
                     whileM $ do
                       idx <- readSTRef idxM
                       idxNode <- MV.unsafeRead nodesM idx
                       let child1 = leftNodeIndex idxNode
                           child2 = rightNodeIndex idxNode
                       child1Node <- MV.unsafeRead nodesM child1   
                       child2Node <- MV.unsafeRead nodesM child2   
                       MV.unsafeWrite nodesM idx $ idxNode
                         { aabb = combine (aabb child1Node) (aabb child2Node)
                         , height = 1 + max (height child1Node) (height child2Node)
                         }
                       let nextIdx = parentNodeIndex idxNode
                       writeSTRef idxM nextIdx
                       return $ nextIdx /= nullNode
                     V.unsafeFreeze nodesM
               in daabbTree' { nodes = nodes'' }
          else let siblingNode' = siblingNode { parentNodeIndex = nullNode }
                   nodes' = V.modify
                            (\v -> MV.unsafeWrite v siblingIndex siblingNode')
                            (nodes daabbTree)
               in freeNode parentIndex $ daabbTree { nodes = nodes' }
  where 
    whileM act = do
      b <- act
      when b $ whileM act

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

validateStructure :: Int -> DAABBTree v a -> ()
validateStructure idx = go [idx] 
  where
    go [] _ = ()
    go (index:xs) daabbTree = 
      if index == nullNode
      then ()
      else if index == rootNodeIndex daabbTree
           then assert (parentNodeIndex (nodes daabbTree V.! index) == nullNode)
                "'index' is the root node but has non-null parent." $
                rest 
           else rest
      where
        rest = let node = nodes daabbTree V.! index
                   child1 = leftNodeIndex node
                   child2 = rightNodeIndex node
               in if isLeaf node
                  then assert (child1 == nullNode)
                       "'node' is a leaf but has a non-null 'leftNodeIndex'." $
                       assert (child2 == nullNode)
                       "'node' is a leaf but has a non-null 'rightNodeIndex'." $
                       assert (height node == 0)
                       "'node' is a leaf but has non-zero 'height'." ()
                  else assert (0 <= child1 && child1 < nodeCapacity daabbTree)
                       "'leftNodeIndex' is not valid." $
                       assert (0 <= child2 && child2 < nodeCapacity daabbTree)
                       "'rightNodeIndex' is not valid." $
                       assert (parentNodeIndex (nodes daabbTree V.! child1) == index)
                       "'leftNodeIndex' of 'index' does not have 'index' as its parent." $
                       assert (parentNodeIndex (nodes daabbTree V.! child2) == index)
                       "'rightNodeIndex' of 'index' does not have 'index' as its parent." $
                       go (child1 : child2 : xs) daabbTree

validateMetrics :: BB v a => Int -> DAABBTree v a -> ()
validateMetrics idx = go [idx]
  where
    go [] _ = ()
    go (index : xs) daabbTree =
      if index == nullNode
      then ()
      else let node = nodes daabbTree V.! index
               child1 = leftNodeIndex node
               child2 = rightNodeIndex node
           in if isLeaf node
              then ()
              else let height1 = height $ nodes daabbTree V.! child1
                       height2 = height $ nodes daabbTree V.! child2
                       heightNode = 1 + max height1 height2
                   in assert (height node == heightNode)
                      "Height of the node is not equal to one plus the max of its childrens' heights." $
                      let aabbNode = combine
                                     (aabb $ nodes daabbTree V.! child1)
                                     (aabb $ nodes daabbTree V.! child2)
                      in assert (aabb node == aabbNode)
                         "Node's aabb is not equal to the combination of its childrens' aabbs." $
                         go (child1 : child2 : xs) daabbTree

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
