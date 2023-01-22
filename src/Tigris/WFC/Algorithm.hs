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

newtype AllTiles = AllTiles (V.Vector Tile) deriving (Semigroup, Monoid)
instance Component AllTiles where
  type Storage AllTiles = ReadOnly (Global AllTiles)

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
                     , ''AllTiles
                     , ''RemainingGrid
                     ]

type WFCSystemT m a = SystemT WFCWorld m a

newtype WFCException = WFCException String deriving (Show)
instance Exception WFCException


leastEntropy :: MonadIO m => WFCSystemT m [(Int, Int)]
leastEntropy = do
  RemainingGrid rgrid <- get global
  let rsizes = V.length <$> rgrid
  case calcEnt rsizes of
    Nothing -> throw $ WFCException "'leastEntroy' found no entroy."
    Just en -> return $ fst en
  where calcEnt =
          M.foldrWithKey
          (\ k a b -> case a of
                        0 -> throw $ WFCException "Zero entropy found by 'leastEntropy'."
                        _ -> case b of
                               Nothing -> Just ([k], a)
                               Just (bk, be) -> case compare a be of
                                                  GT -> b
                                                  EQ -> Just (k:bk, a)
                                                  LT -> Just ([k], a)
          )
          Nothing

randomCell :: MonadIO m => [(Int, Int)] -> WFCSystemT m (Int, Int)
randomCell [] = throw $ WFCException "'randomCell' recieved an empty list"
randomCell l = do
  let leng = length l
  choice <- randomRIO (0, leng - 1)
  return $ l !! choice
    
weightedChoice :: MonadIO m => (Int, Int) -> WFCSystemT m ()
weightedChoice cell = do
  RemainingGrid rgrid <- get global
  let tiles = rgrid M.! cell
      weightList :: [(Double, Tile)]
      weightList = [ (fromIntegral $ weight tile, tile) | tile <- V.toList tiles ]
      tileRVar = weightedCategorical weightList
  thing <- fmap (runState (sampleStateRVar tileRVar)) getStdGen
  --return (cell, fst thing)
  modify global $ \(Grid grid) -> Grid $ M.insert cell (fst thing) grid
  modify global $ \(RemainingGrid nrgrid) -> RemainingGrid $ M.delete cell nrgrid
  return ()


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

wave :: MonadIO m => WFCSystemT m Grid
wave = do
  RemainingGrid rgrid <- get global
  if M.null rgrid
    then get global
    else do cs <- leastEntropy
            c <- randomCell cs
            weightedChoice c
            remainingGrid [c]
            wave

_wfc :: V.Vector Tile -> (Int, Int) -> Maybe Grid -> IO Grid
_wfc tiles size@(a,b) mgrid = do
  w <- initWFCWorld
  runWith w $ do
    setReadOnly global $ AllTiles tiles
    setReadOnly global $ MapSize size
    let keys = [ (x,y) | x <- [0..(a-1)], y <- [0..(b-1)] ]
        remList = (, tiles) <$> keys
    set global $ RemainingGrid $ M.fromList remList
    case mgrid of
      Nothing -> return ()
      Just grid -> do set global grid
                      let gridKeys (Grid g) = M.keys g
                      remainingGrid $ gridKeys grid
    wave
    
wfc :: V.Vector Tile -> (Int, Int) -> Maybe Grid -> IO Grid
wfc tiles size mgrid = do
  r :: Either WFCException Grid <- try $ _wfc tiles size mgrid
  case r of
    Left _ -> wfc tiles size mgrid
    Right grid -> return grid
