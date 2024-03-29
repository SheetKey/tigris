{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Tigris.WFC.Algorithm
  ( wfc
  , Tile (..)
  , Grid (..)
  ) where

-- containers
import qualified Data.Map.Strict as M

-- vector
import qualified Data.Vector as V

-- import Apecs
import Apecs
import Apecs.Stores

-- base
import Control.Monad.IO.Class
import Control.Exception 
import Data.Foldable (foldr')

-- random
import System.Random (randomRIO, getStdGen)

-- random-fu
import Data.Random.Distribution.Categorical (weightedCategorical)

-- rvar
import Data.RVar (sampleStateRVar)

-- transformers
import Control.Monad.Trans.State (runState)

  
data Tile = Tile
  { tileId :: Int
  , nconnector :: Int
  , econnector :: Int
  , sconnector :: Int
  , wconnector :: Int
  , weight :: Int
  }
  deriving (Eq, Show)

newtype Grid = Grid (M.Map (Int, Int) Tile) deriving (Semigroup, Monoid, Show)
instance Component Grid where
  type Storage Grid = Global Grid

newtype MapSize = MapSize (Int, Int) 
instance Semigroup MapSize where
  _ <> _ = error "'<>' should not be used for 'MapSize'."
instance Monoid MapSize where
  mempty = MapSize (0, 0)
instance Component MapSize where
  type Storage MapSize = ReadOnly (Global MapSize)

newtype RemainingGrid = RemainingGrid (M.Map (Int, Int) (V.Vector Tile)) deriving (Semigroup, Monoid, Show)
instance Component RemainingGrid where
  type Storage RemainingGrid = Global RemainingGrid

makeWorld "WFCWorld" [ ''Grid
                     , ''MapSize
                     , ''RemainingGrid
                     ]

type WFCSystemT m a = SystemT WFCWorld m a

newtype WFCException = WFCException String deriving (Show)
instance Exception WFCException

leastEntropyWith :: (MonadIO m, Num a, Ord a) => (V.Vector Tile -> a) -> WFCSystemT m [(Int, Int)]
leastEntropyWith tileToOrd = do
  RemainingGrid rgrid <- get global
  case calcEntropy (tileToOrd <$> rgrid) of
    Nothing -> throw $ WFCException "'leastEntropy' found no entropy."
    Just en -> return $ fst en
  where
    calcEntropy =
      M.foldrWithKey
      (\key orderedVal acc -> if orderedVal == 0
        then throw $ WFCException "Zero entropy found by 'leastEntropy'."
        else case acc of
               Nothing                  -> Just ([key], orderedVal)
               Just (keyList, leastVal) -> case compare orderedVal leastVal of
                                             GT -> acc
                                             EQ -> Just (key : keyList, orderedVal)
                                             LT -> Just ([key]        , orderedVal)
      )
      Nothing

cellEntropy :: V.Vector Tile -> Float
cellEntropy tiles = log weightSum - (weightLogWeightsSum / weightSum)
  where
    weights = (fromIntegral . weight) <$> tiles
    weightSum = V.sum weights
    weightLogWeights = (\w -> w * log w) <$> weights
    weightLogWeightsSum = V.sum weightLogWeights

leastEntropy :: MonadIO m => WFCSystemT m [(Int, Int)]
leastEntropy = leastEntropyWith cellEntropy

randomCell :: MonadIO m => [(Int, Int)] -> WFCSystemT m (Int, Int)
randomCell [] = throw $ WFCException "'randomCell' recieved an empty list"
randomCell l = do
  let leng = length l
  choice <- randomRIO (0, leng - 1)
  return $ l !! choice
    
weightedChoice :: MonadIO m => (Int, Int) -> WFCSystemT m ((Int, Int), Tile)
weightedChoice cell = do
  RemainingGrid rgrid <- get global
  let tiles = rgrid M.! cell
      weightList :: [(Double, Tile)]
      weightList = [ (fromIntegral $ weight tile, tile) | tile <- V.toList tiles ]
      tileRVar = weightedCategorical weightList
  (tile, _) <- fmap (runState (sampleStateRVar tileRVar)) getStdGen
  modify global $ \(Grid grid) -> Grid $ M.insert cell tile grid
  modify global $ \(RemainingGrid nrgrid) -> RemainingGrid $ M.delete cell nrgrid
  return (cell, tile)

applyInRadius :: MonadIO m => Int -> (Tile -> (Int, Int) -> V.Vector Tile -> V.Vector Tile) -> ((Int, Int), Tile) -> WFCSystemT m ()
applyInRadius radius f ((cellX, cellY), tile) = do
  RemainingGrid rgrid <- get global
  let cellOffsets = [ (x, y) | x <- [(-radius)..radius]
                             , y <- [(-radius)..radius]
                             , (x * x) + (y * y) <= radius * radius
                             ]
      rgrid' = foldr' (\offset@(offX, offY) grid ->
                         let cell = (cellX + offX, cellY + offY)
                         in case grid M.!? cell of
                              Just tiles -> M.insert cell (f tile offset tiles) grid
                              Nothing    -> grid
                      )
               rgrid
               cellOffsets
  set global $ RemainingGrid rgrid'

remainingGrid :: MonadIO m => [(Int, Int)] -> WFCSystemT m ()
remainingGrid [] = return ()
remainingGrid (c@(x,y):cs) = do
  Grid grid <- get global
  RemainingGrid rgrid <- get global
  case (grid M.!? c, rgrid M.!? c) of
    -- the input cell has already collapsed
    (Just tile, Nothing)  -> do
      let
        -- takes a neightbor cell to the incomming cell,
        -- the directional connector,
        -- and a function to get the new cells directional connector
        -- (these dirs should be opposite)
        propDir (a,b) conn f = case rgrid M.!? (a,b) of
          -- new cell not in remaining grid return empty list
          Nothing -> return []
          -- new cell in remaining grid
          -- modify the remaining grip by filtering the vector of possible tiles
          -- so that the connectors match
          Just tiles ->
            let newTiles = V.filter (\d -> f d == conn) tiles
            in if newTiles == tiles
               then return []
               else do modify global $ \(RemainingGrid nrgrid) ->
                                         RemainingGrid $ M.insert (a,b) newTiles nrgrid
                       return [(a,b)]
      d1 <- propDir (x, y+1) (nconnector tile) sconnector
      d2 <- propDir (x, y-1) (sconnector tile) nconnector
      d3 <- propDir (x+1, y) (econnector tile) wconnector
      d4 <- propDir (x-1, y) (wconnector tile) econnector
      -- propagate to the neighboring cells if there are any (may be []), and the remaining cs
      remainingGrid $ d1 ++ d2 ++ d3 ++ d4 ++ cs

    -- the input cell has not been collapsed
    (Nothing, Just tiles) -> do
      let
        propDir (a,b) conns f = case rgrid M.!? (a,b) of
          Nothing -> return []
          Just dirtiles ->
            let newTiles = V.filter (\d -> f d `V.elem` conns) dirtiles
            in if dirtiles == newTiles
               then return []
               else do modify global $ \(RemainingGrid nrgrid) ->
                                         RemainingGrid $ M.insert (a,b) newTiles nrgrid
                       return [(a,b)]
      d1 <- propDir (x, y+1) (V.uniq $ nconnector <$> tiles) sconnector
      d2 <- propDir (x, y-1) (V.uniq $ sconnector <$> tiles) nconnector
      d3 <- propDir (x+1, y) (V.uniq $ econnector <$> tiles) wconnector
      d4 <- propDir (x-1, y) (V.uniq $ wconnector <$> tiles) econnector
      remainingGrid $ d1 ++ d2 ++ d3 ++ d4 ++ cs
      
    _ -> throw $ WFCException "'remainingGrid' error"

wave :: MonadIO m => Int -> (Tile -> (Int, Int) -> V.Vector Tile -> V.Vector Tile) -> WFCSystemT m Grid
wave radius f = go
  where
    go = do 
      RemainingGrid rgrid <- get global
      if M.null rgrid
        then get global
        else do cs <- leastEntropy
                c <- randomCell cs
                result <- weightedChoice c
                applyInRadius radius f result
                remainingGrid [c]
                wave radius f

_wfc :: V.Vector Tile -> (Int, Int) -> Maybe Grid -> Int -> (Tile -> (Int, Int) -> V.Vector Tile -> V.Vector Tile) -> IO Grid
_wfc tiles size@(a,b) mgrid radius f = do
  w <- initWFCWorld
  runWith w $ do
    setReadOnly global $ MapSize size
    let keys = [ (x,y) | x <- [0..(a-1)], y <- [0..(b-1)] ]
        remList = (, tiles) <$> keys
    set global $ RemainingGrid $ M.fromList remList
    case mgrid of
      Nothing -> return ()
      Just grid -> do set global grid
                      let gridKeys (Grid g) = M.keys g
                      remainingGrid $ gridKeys grid
    wave radius f
    
wfc :: V.Vector Tile -> (Int, Int) -> Maybe Grid -> Int -> (Tile -> (Int, Int) -> V.Vector Tile -> V.Vector Tile) -> IO Grid
wfc tiles size mgrid radius f = do
  r :: Either WFCException Grid <- try $ _wfc tiles size mgrid radius f
  case r of
    Left _ -> wfc tiles size mgrid radius f
    Right grid -> return grid
