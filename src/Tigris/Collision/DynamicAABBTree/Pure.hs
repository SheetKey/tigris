{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Tigris.Collision.DynamicAABBTree.Pure
  ( initDAABBTree
  , insertObject
  , removeObject
  , updateObject
  , validate
  ) where

-- tigris
import Tigris.Collision.AABB
import Tigris.Collision.DynamicAABBTree.Type

-- vector
import qualified Data.Vector as V

-- containers
import Data.IntMap.Strict as IM

-- pqueue
import qualified Data.PQueue.Max as H

-- base
import Control.Monad.ST
import Data.STRef
import Control.Monad (when)
import Data.Functor.Identity

nullNodes :: BB v a => Int -> Int -> V.Vector (Node v a)
nullNodes offSet size = V.generate size $ \i -> if i /= size - 1
  then nullTreeNode { nextNodeIndex = offSet + i + 1 }
  else nullTreeNode

initDAABBTree :: BB v a => Int -> Int -> a -> DAABBTree v a
initDAABBTree nodeCapacity growthSize aabbFatExtension = DAABBTree
  { rootNodeIndex     = 0
  , nodeCount         = 0
  , nodeCapacity      = nodeCapacity
  , growthSize        = growthSize
  , nextFreeNodeIndex = 0
  , nodes             = nullNodes 0 nodeCapacity
  , aabbFatExtension  = aabbFatExtension
  , objectIndexMap    = IM.empty
  }

assert :: Bool -> String -> a -> a
assert tf str a = if tf then a else error str

growNodes :: BB v a => DAABBTree v a -> DAABBTree v a
growNodes DAABBTree {..} =
  assert (nodeCount == nodeCapacity)
  "'growNodes' called but 'nodeCount /= nodeCapacity'." $
  let addedNodes = nullNodes nodeCount growthSize
  in DAABBTree
     { nodes = nodes V.++ addedNodes
     , nextFreeNodeIndex = nodeCount
     , nodeCapacity = nodeCapacity + growthSize
     , ..
     }

allocateNode :: BB v a => DAABBTree v a -> (Int, DAABBTree v a)
allocateNode daabbTree = if nextFreeNodeIndex daabbTree == nullNode
  then peel $ growNodes daabbTree
  else peel daabbTree
  where
    peel daabbTree =
      let nodeIndex = nextFreeNodeIndex daabbTree
          node = nodes daabbTree V.! nodeIndex
          node' = node
                  { object = nullNode
                  , parentNodeIndex = nullNode
                  , leftNodeIndex = nullNode
                  , rightNodeIndex = nullNode
                  , height = 0
                  , isLeaf = False
                  }
          nodes' = nodes daabbTree V.// [(nodeIndex, node')]
      in ( nodeIndex
         , daabbTree
           { nextFreeNodeIndex = nextNodeIndex node
           , nodeCount = nodeCount daabbTree + 1
           , nodes = nodes'
           }
         )

freeNode :: Int -> DAABBTree v a -> DAABBTree v a
freeNode nodeIndex DAABBTree {..} =
  assert (0 <= nodeIndex && nodeIndex <= nodeCapacity)
  "'freeNode' called but 'nodeIndex' not within valid bounds." $
  assert (0 < nodeCount)
  "'freeNode' called but 'nodeCount' is not larger than 0." $
  let node = nodes V.! nodeIndex
      node' = node { nextNodeIndex = nextFreeNodeIndex, height = nullNode }
  in DAABBTree
     { nodes = nodes V.// [(nodeIndex, node')]
     , nextFreeNodeIndex = nodeIndex
     , nodeCount = nodeCount - 1
     , ..
     }
    
insertObject :: BB v a => Int -> AABB v a -> DAABBTree v a -> DAABBTree v a
insertObject objId objaabb daabbTree =
  let (objIndex, daabbTree') = allocateNode daabbTree
      objNode = nodes daabbTree' V.! objIndex
      objNode' = objNode
                 { aabb = fattenAABB (aabbFatExtension daabbTree') objaabb
                 , height = 0
                 , object = objId
                 , isLeaf = True
                 }
      nodes' = nodes daabbTree' V.// [(objIndex, objNode')]
  in insertLeaf objIndex $ daabbTree'
     { nodes = nodes'
     , objectIndexMap = IM.insert objId objIndex $ objectIndexMap daabbTree' 
     }

removeObject :: BB v a => Int -> DAABBTree v a -> DAABBTree v a
removeObject objId DAABBTree {..} =
  let objIndex = objectIndexMap IM.! objId
  in assert (0 <= objIndex && objIndex < nodeCapacity)
     "'objIndex' in 'objectIndexMap' is out of bounds." $
     assert (isLeaf $ nodes V.! objIndex)
     "'objIndex' in 'objectIndexMap' is not a leaf node in the tree." $
    freeNode objIndex $ removeLeaf objIndex $ DAABBTree
     { objectIndexMap = IM.delete objId (objectIndexMap), .. }

updateObject :: BB v a => Int -> AABB v a -> DAABBTree v a-> DAABBTree v a
updateObject objId objAABB daabbTree = 
  let objIndex = objectIndexMap daabbTree IM.! objId
  in assert (0 <= objIndex && objIndex < nodeCapacity daabbTree)
     "'objIndex' in 'objectIndexMap' is out of bounds." $
     assert (isLeaf $ nodes daabbTree V.! objIndex)
     "'objIndex' in 'objectIndexMap' is not a leaf node in the tree." $
     let r = aabbFatExtension daabbTree
         fatAABB = fattenAABB r objAABB
         hugeAABB = fattenAABB (4 * r) fatAABB
         objNode = nodes daabbTree V.! objIndex
         objNodeAABB = aabb objNode
     in if (objNodeAABB `contains` objAABB && hugeAABB `contains` objNodeAABB)
        then daabbTree
        else let daabbTree' = removeLeaf objIndex daabbTree
                 newObjNode = objNode { aabb = fatAABB }
                 nodes' = nodes daabbTree' V.// [(objIndex, newObjNode)]
             in insertLeaf objIndex (daabbTree' { nodes = nodes' })

data Candidate a = Candidate
  { candidateIndex :: Int
  , inheritanceCost :: a
  }
  deriving (Show, Eq)

instance Ord a => Ord (Candidate a) where
  Candidate _ c1 <= Candidate _ c2 = c1 <= c2

findBestSibling :: BB v a => AABB v a -> a -> DAABBTree v a -> Int
findBestSibling aabbL areaL DAABBTree {..} =
    let initBestCost = area $ combine (aabb $ nodes V.! rootNodeIndex) aabbL
        initCandidate = Candidate rootNodeIndex (initBestCost - area (aabb $ nodes V.! rootNodeIndex))
    in go (H.singleton initCandidate) rootNodeIndex initBestCost
  where
    go heap bestSibling bestCost = case H.maxView heap of
      Nothing -> bestSibling
      Just (candidate, rest) ->
        let index = candidateIndex candidate
            lowerBoundCost = inheritanceCost candidate + areaL
        in if lowerBoundCost > bestCost
           then bestSibling
           else let node = nodes V.! index
                in if isLeaf node
                   then go rest bestSibling bestCost
                   else let child1 = leftNodeIndex node
                            child2 = rightNodeIndex node
                            directCost1 = area $ combine (aabb $ nodes V.! child1) aabbL
                            directCost2 = area $ combine (aabb $ nodes V.! child2) aabbL
                            totalCost1 = directCost1 + inheritanceCost candidate
                            totalCost2 = directCost2 + inheritanceCost candidate
                            (bestCost', bestSibling') = if totalCost1 <= bestCost
                                                        then if totalCost2 <= totalCost1
                                                             then (totalCost2, child2)
                                                             else (totalCost1, child1)
                                                        else if totalCost2 <= bestCost
                                                             then (totalCost2, child2)
                                                             else (bestCost, bestSibling)
                            inheritanceCost1 = totalCost1 - area (aabb $ nodes V.! child1)
                            inheritanceCost2 = totalCost2 - area (aabb $ nodes V.! child2)
                            heap1 = if inheritanceCost1 + areaL <= bestCost'
                                    then H.insert (Candidate child1 inheritanceCost1) rest
                                    else rest
                            heap2 = if inheritanceCost2 + areaL <= bestCost'
                                    then H.insert (Candidate child2 inheritanceCost2) heap1
                                    else heap1
                        in go heap2 bestSibling' bestCost'

findNewParent :: BB v a => AABB v a -> Int -> Int -> DAABBTree v a -> (Int, DAABBTree v a)
findNewParent aabbL leafIndex sibling daabbTree =
  let oldParent = parentNodeIndex $ nodes daabbTree V.! sibling
      (newParent, daabbTree') = allocateNode daabbTree
      newParentNode = (nodes daabbTree' V.! newParent)
                      { parentNodeIndex = oldParent
                      , aabb = combine aabbL (aabb $ nodes daabbTree' V.! sibling)
                      , height = 1 + height (nodes daabbTree' V.! sibling)
                      , leftNodeIndex = sibling
                      , rightNodeIndex = leafIndex
                      }
      siblingNode = (nodes daabbTree' V.! sibling)
                    { parentNodeIndex = newParent }
      leafNode = (nodes daabbTree' V.! leafIndex)
                 { parentNodeIndex = newParent }
  in if oldParent /= nullNode -- sibling was not the root
     then let oldParentNode = if (leftNodeIndex $ nodes daabbTree' V.! oldParent) == sibling
                              then (nodes daabbTree' V.! oldParent)
                                   { leftNodeIndex = newParent }
                              else (nodes daabbTree' V.! oldParent)
                                   { rightNodeIndex = newParent }
              nodes' = (nodes daabbTree') V.//
                       [ (oldParent, oldParentNode)
                       , (newParent, newParentNode)
                       , (sibling, siblingNode)
                       , (leafIndex, leafNode)
                       ]
          in (oldParent, daabbTree' { nodes = nodes' })
     else let nodes' = (nodes daabbTree') V.//
                       [ (newParent, newParentNode)
                       , (sibling, siblingNode)
                       , (leafIndex, leafNode)
                       ]
          in (oldParent, daabbTree' { nodes = nodes', rootNodeIndex = newParent })

fixTree :: BB v a => (Int, DAABBTree v a) -> DAABBTree v a
fixTree (oldParent, tree) = go oldParent tree
  where
    go index daabbTree = if index == nullNode
      then daabbTree
      else let child1 = leftNodeIndex $ nodes daabbTree V.! index
               child2 = rightNodeIndex $ nodes daabbTree V.! index
           in assert (child1 /= nullNode)
             "'child1 == nullNode' when fixing the tree." $
              assert (child2 /= nullNode)
              "'child2 == nullNode' when fixing the tree." $
              let indexNode = (nodes daabbTree V.! index)
                              { height = 1 + max
                                         (height $ nodes daabbTree V.! child1)
                                         (height $ nodes daabbTree V.! child2)
                              , aabb = combine
                                       (aabb $ nodes daabbTree V.! child1)
                                       (aabb $ nodes daabbTree V.! child2)
                              }
                  daabbTree' = rotate index (daabbTree { nodes = nodes daabbTree V.// [(index, indexNode)] })
              in go (parentNodeIndex $ nodes daabbTree' V.! index) daabbTree'

insertLeaf :: BB v a => Int -> DAABBTree v a-> DAABBTree v a
insertLeaf leafIndex daabbTree = if rootNodeIndex daabbTree == nullNode
  then let leafNode = nodes daabbTree V.! leafIndex
           leafNode' = leafNode { parentNodeIndex = nullNode }
       in daabbTree
          { rootNodeIndex = leafIndex
          , nodes = nodes daabbTree V.// [(leafIndex, leafNode')]
          }
  else let aabbL = aabb $ nodes daabbTree V.! leafIndex
           areaL = area aabbL
           sibling = findBestSibling aabbL areaL daabbTree
       in fixTree $ findNewParent aabbL leafIndex sibling daabbTree
  

removeLeaf :: BB v a => Int -> DAABBTree v a -> DAABBTree v a
removeLeaf leafIndex daabbTree = if leafIndex == rootNodeIndex daabbTree
  then daabbTree { rootNodeIndex = nullNode }
  else let parentIndex = parentNodeIndex $ nodes daabbTree V.! leafIndex
           parentNode = nodes daabbTree V.! parentIndex
           grandParentIndex = parentNodeIndex parentNode
           siblingIndex = if leftNodeIndex parentNode == leafIndex
                          then rightNodeIndex parentNode
                          else leftNodeIndex parentNode
       in if grandParentIndex /= nullNode
          then let grandParentNode = nodes daabbTree V.! grandParentIndex
                   grandParentNode' = if leftNodeIndex grandParentNode == parentIndex
                                      then grandParentNode { leftNodeIndex = siblingIndex }
                                      else grandParentNode { rightNodeIndex = siblingIndex }
                   siblingNode = (nodes daabbTree V.! siblingIndex)
                                 { parentNodeIndex = grandParentIndex }
                   nodes' = (nodes daabbTree) V.//
                            [ (grandParentIndex, grandParentNode')
                            , (siblingIndex, siblingNode)
                            ]
                   daabbTree' = freeNode parentIndex $ daabbTree
                                { nodes = nodes' }
                   in adjustAncestorBounds grandParentIndex daabbTree'
          else let siblingNode = (nodes daabbTree V.! siblingIndex)
                                 { parentNodeIndex = nullNode }
                   nodes' = nodes daabbTree V.// [(siblingIndex, siblingNode)]
               in freeNode parentIndex $ daabbTree { nodes = nodes', rootNodeIndex = siblingIndex }
  where
    adjustAncestorBounds index tree = if index == nullNode
      then tree
      else let child1 = leftNodeIndex $ nodes tree V.! index
               child2 = rightNodeIndex $ nodes tree V.! index
           in assert (child1 /= nullNode)
              "'child1 == nullNode' when adjusting ancestor bounds." $
              assert (child2 /= nullNode)
              "'child2 == nulNlNode' when adjusting ancestor bounds." $
              let node = (nodes tree V.! index)
                         { aabb = combine
                                  (aabb $ nodes tree V.! child1)
                                  (aabb $ nodes tree V.! child2)
                         , height = 1 + max
                                    (height $ nodes tree V.! child1)
                                    (height $ nodes tree V.! child2)
                         }
                  nodes' = nodes tree V.// [(index, node)]
              in adjustAncestorBounds (parentNodeIndex node) $ tree { nodes = nodes' }

rotHeight0 :: BB v a => Int -> Node v a -> Int -> Node v a -> Int -> Node v a -> DAABBTree v a -> DAABBTree v a
rotHeight0 iA nA leaf leafNode sibling siblingNode DAABBTree {..} =
  assert (height siblingNode > 0)
  "'height siblingNode' is not greater than 0 but the sibling of 'siblingNode' is a leaf and the parent of 'siblingNode' has height larger than 2." $
  let i1 = leftNodeIndex siblingNode
      i2 = rightNodeIndex siblingNode
  in assert (0 <= i1 && i1 < nodeCapacity)
     "'i1' not in bounds." $
     assert (0 <= i2 && i2 < nodeCapacity)
     "'i2' not in bounds." $
  let n1 = nodes V.! i1
      n2 = nodes V.! i2
      baseCost = area $ aabb siblingNode
      -- cost of swapping leaf and i1
      aabbL2 = combine (aabb leafNode) (aabb n2)
      costL1 = area aabbL2
      -- cost of swapping leaf and i2
      aabbL1 = combine (aabb leafNode) (aabb n1)
      costL2 = area aabbL1
  in if baseCost < costL1 && baseCost < costL2
  then DAABBTree {..}
  else if costL1 < costL2
  then -- swap leaf and i1
    let heightSibling = 1 + max (height leafNode) (height n2)
        heightA = 1 + max heightSibling (height n1)
        nA' = nA { leftNodeIndex = i1, height = heightA }
        siblingNode' = siblingNode { leftNodeIndex = leaf, aabb = aabbL2, height = heightSibling }
        leafNode' = leafNode { parentNodeIndex = sibling }
        n1' = n1 { parentNodeIndex = iA }
        nodes' = nodes V.//
                 [ (iA, nA')
                 , (sibling, siblingNode')
                 , (leaf, leafNode')
                 , (i1, n1')
                 ]
    in DAABBTree { nodes = nodes', .. }
  else let heightSibling = 1 + max (height leafNode) (height n1)
           heightA = 1 + max heightSibling (height n2)
           nA' = nA { leftNodeIndex = i2, height = heightA }
           siblingNode' = siblingNode { rightNodeIndex = leaf, aabb = aabbL1, height = heightSibling }
           leafNode' = leafNode { parentNodeIndex = sibling }
           n2' = n2 { parentNodeIndex = iA }
           nodes' = nodes V.//
                    [ (iA, nA')
                    , (sibling, siblingNode')
                    , (leaf, leafNode')
                    , (i2, n2')
                    ]
       in DAABBTree { nodes = nodes', .. }

rotHeightB0 :: BB v a => Int -> Node v a -> Int -> Node v a -> Int -> Node v a -> DAABBTree v a -> DAABBTree v a
rotHeightB0 = rotHeight0

rotHeightC0 :: BB v a => Int -> Node v a -> Int -> Node v a -> Int -> Node v a -> DAABBTree v a -> DAABBTree v a
rotHeightC0 iA nA iB nB iC nC = rotHeight0 iA nA iC nC iB nB

rotElse :: BB v a => Int -> Node v a -> Int -> Node v a -> Int -> Node v a -> DAABBTree v a -> DAABBTree v a
rotElse iA nA iB nB iC nC DAABBTree {..} = 
  let iD = leftNodeIndex nB
      iE = rightNodeIndex nB
      iF = leftNodeIndex nC
      iG = rightNodeIndex nC
  in assert (0 <= iD && iD < nodeCapacity)
     "'iD' not in bounds." $
     assert (0 <= iE && iE < nodeCapacity)
     "'iE' not in bounds." $
     assert (0 <= iF && iF < nodeCapacity)
     "'iF' not in bounds." $
     assert (0 <= iG && iG < nodeCapacity)
     "'iG' not in bounds." $
  let nD = nodes V.! iD
      nE = nodes V.! iE
      nF = nodes V.! iF
      nG = nodes V.! iG
      -- base cost
      areaB = area $ aabb nB
      areaC = area $ aabb nC
      baseCost = areaB + areaC
      -- cost of swapping B and F
      aabbBG = combine (aabb nB) (aabb nG)
      costBF = areaB + area aabbBG
      -- cost of swapping B and G
      aabbBF = combine (aabb nB) (aabb nF)
      costBG = areaB + area aabbBF
      -- cost of swapping C and D
      aabbCE = combine (aabb nC) (aabb nE)
      costCD = areaC + area aabbCE
      -- cost of swapping C and E
      aabbCD = combine (aabb nC) (aabb nD)
      costCE = areaC + area aabbCD
      minCost = minimum [baseCost, costBF, costBG, costCD, costCE]
  in if | minCost == baseCost -> DAABBTree {..}
        | minCost == costBF ->
            let heightC = 1 + max (height nB) (height nG)
                heightA = 1 + max heightC (height nF)
                nA' = nA { leftNodeIndex = iF, height = heightA }
                nC' = nC { leftNodeIndex = iB, aabb = aabbBG, height = heightC }
                nB' = nB { parentNodeIndex = iC }
                nF' = nF { parentNodeIndex = iA }
                nodes' = nodes V.//
                         [ (iA, nA')
                         , (iC, nC')
                         , (iB, nB')
                         , (iF, nF')
                         ]
            in DAABBTree { nodes = nodes', .. }
        | minCost == costBG ->
            let heightC = 1 + max (height nB) (height nF)
                heightA = 1 + max heightC (height nG)
                nA' = nA { leftNodeIndex = iG, height = heightA }
                nC' = nC { rightNodeIndex = iB, aabb = aabbBF, height = heightC }
                nB' = nB { parentNodeIndex = iC }
                nG' = nG { parentNodeIndex = iA }
                nodes' = nodes V.//
                         [ (iA, nA')
                         , (iC, nC')
                         , (iB, nB')
                         , (iG, nG')
                         ]
            in DAABBTree { nodes = nodes', .. }
        | minCost == costCD ->
            let heightB = 1 + max (height nC) (height nE)
                heightA = 1 + max heightB (height nD)
                nA' = nA { rightNodeIndex = iD, height = heightA }
                nB' = nB { leftNodeIndex = iC, aabb = aabbCE, height = heightB }
                nC' = nC { parentNodeIndex = iB }
                nD' = nD { parentNodeIndex = iA }
                nodes' = nodes V.//
                         [ (iA, nA')
                         , (iB, nB')
                         , (iC, nC')
                         , (iD, nD')
                         ]
            in DAABBTree { nodes = nodes', .. }
        | minCost == costCE ->
            let heightB = 1 + max (height nC) (height nD)
                heightA = 1 + max heightB (height nE)
                nA' = nA { rightNodeIndex = iE, height = heightA }
                nB' = nB { rightNodeIndex = iC, aabb = aabbCD, height = heightB }
                nC' = nC { parentNodeIndex = iB }
                nE' = nE { parentNodeIndex = iA }
                nodes' = nodes V.//
                         [ (iA, nA')
                         , (iB, nB')
                         , (iC, nC')
                         , (iE, nE')
                         ]
            in DAABBTree { nodes = nodes', .. }
        | otherwise -> error "this cannot happen"

rotate :: BB v a => Int -> DAABBTree v a -> DAABBTree v a
rotate iA DAABBTree {..} =
  assert (iA /= nullNode)
  "'iA == nullNode' when rotating the tree." $
  let nA = nodes V.! iA
  in if height nA < 2
  then DAABBTree {..}
  else let iB = leftNodeIndex nA
           iC = rightNodeIndex nA
  in assert (0 <= iB && iB < nodeCapacity)
     "'iB' not in bounds." $
     assert (0 <= iC && iC < nodeCapacity)
     "'iC' not in bounds." $
  let nB = nodes V.! iB
      nC = nodes V.! iC
  in if height nB == 0
     then rotHeightB0 iA nA iB nB iC nC DAABBTree {..}
     else if height nC == 0
          then rotHeightC0 iA nA iB nB iC nC DAABBTree {..}
          else rotElse iA nA iB nB iC nC DAABBTree {..}
            
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
