{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Tigris.WFC.Algorithm where

-- containers
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

-- vector
import qualified Data.Vector as V

-- import Apecs
import Apecs
import Apecs.Stores

-- base
import Control.Monad (foldM)
import Data.List (intersect)
import Control.Monad.IO.Class

data Tile = Tile
  { tileId :: TileId
  , nconnector :: Int
  , econnector :: Int
  , sconnector :: Int
  , wconnector :: Int
  , weight :: Int
  }

newtype TileId = TileId Int deriving (Eq, Ord)

type Entropy = M.Map (Int, Int) Int

newtype AllTiles = AllTiles (V.Vector TileId) deriving (Semigroup, Monoid)
instance Component AllTiles where
  type Storage AllTiles = ReadOnly (Global AllTiles)

newtype Grid = Grid (M.Map (Int, Int) TileId) deriving (Semigroup, Monoid)
instance Component Grid where
  type Storage Grid = Global Grid

newtype TileFromId = TileFromId (M.Map TileId Tile) deriving (Semigroup, Monoid)
instance Component TileFromId where
  type Storage TileFromId = ReadOnly (Global TileFromId)

newtype MapSize = MapSize (Int, Int) 
instance Semigroup MapSize where
  _ <> _ = error "'<>' should not be used for 'MapSize'."
instance Monoid MapSize where
  mempty = MapSize (0, 0)
instance Component MapSize where
  type Storage MapSize = Global MapSize

newtype TileAdjacencyN = TileAdjacencyN (IM.IntMap (V.Vector TileId)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyN where
  type Storage TileAdjacencyN = ReadOnly (Global TileAdjacencyN)

newtype TileAdjacencyE = TileAdjacencyE (IM.IntMap (V.Vector TileId)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyE where
  type Storage TileAdjacencyE = ReadOnly (Global TileAdjacencyE)

newtype TileAdjacencyS = TileAdjacencyS (IM.IntMap (V.Vector TileId)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyS where
  type Storage TileAdjacencyS = ReadOnly (Global TileAdjacencyS)

newtype TileAdjacencyW = TileAdjacencyW (IM.IntMap (V.Vector TileId)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyW where
  type Storage TileAdjacencyW = ReadOnly (Global TileAdjacencyW)

makeWorld "WFCWorld" [ ''Grid
                     , ''TileFromId
                     , ''MapSize
                     , ''AllTiles
                     , ''TileAdjacencyN
                     , ''TileAdjacencyE
                     , ''TileAdjacencyS
                     , ''TileAdjacencyW
                     ]

type WFCSystemT m a = SystemT WFCWorld m a

-- inputs neightboring tiles
possibleTiles :: MonadIO m
              => (Maybe TileId, Maybe TileId, Maybe TileId, Maybe TileId)
              -> WFCSystemT m (V.Vector TileId)
possibleTiles (n, e, s, w) = do
  TileFromId tmap <- get global
  let _connectN = case n of
                    Nothing  -> return Nothing
                    Just nid -> return $ Just $ sconnector $ tmap M.! nid
      _connectE = case e of
                    Nothing  -> return Nothing
                    Just eid -> return $ Just $ wconnector $ tmap M.! eid
      _connectS = case s of
                    Nothing  -> return Nothing
                    Just sid -> return $ Just $ nconnector $ tmap M.! sid
      _connectW = case w of
                    Nothing  -> return Nothing
                    Just wid -> return $ Just $ econnector $ tmap M.! wid
  connectN <- _connectN
  connectE <- _connectE
  connectS <- _connectS
  connectW <- _connectW
  AllTiles vtiles <- get global
  let _possibleN = case connectN of
                     Nothing   -> return vtiles
                     Just conn -> do TileAdjacencyN ntiles <- get global
                                     return $ ntiles IM.! conn
      _possibleE = case connectE of
                     Nothing   -> return vtiles
                     Just conn -> do TileAdjacencyE etiles <- get global
                                     return $ etiles IM.! conn
      _possibleS = case connectS of
                     Nothing   -> return vtiles
                     Just conn -> do TileAdjacencyS stiles <- get global
                                     return $ stiles IM.! conn
      _possibleW = case connectW of
                     Nothing   -> return vtiles
                     Just conn -> do TileAdjacencyW wtiles <- get global
                                     return $ wtiles IM.! conn
  possibleN <- _possibleN
  possibleE <- _possibleE
  possibleS <- _possibleS
  possibleW <- _possibleW
  return
    $ intersectVecs possibleW
    $ intersectVecs possibleS
    $ intersectVecs possibleE possibleN


intersectVecs :: Eq a => V.Vector a -> V.Vector a -> V.Vector a
intersectVecs v1 v2 = if V.length v1 <= V.length v2
                      then go v1 v2 $ V.length v1 - 1
                      else go v2 v1 $ V.length v2 - 1
        -- go: Is called with the shorter vector first and the length of the shorter
        -- vector - 1.
  where go :: Eq a => V.Vector a -> V.Vector a -> Int -> V.Vector a
        go vs vl 0 = if (vs V.! 0) `V.elem` vl
                     then V.singleton $ vs V.! 0
                     else V.empty
        go vs vl n = if (vs V.! n) `V.elem` vl
                     then (vs V.! n) `V.cons` go vs vl (n - 1)
                     else go vs vl (n - 1)


makeEntropy :: MonadIO m => Grid -> WFCSystemT m Entropy
makeEntropy (Grid grid) = do
  MapSize (xMax, yMax) <- get global
  let gridKeys = M.keys grid
      keys = [(x, y) | x <- [0..xMax], y <- [0..yMax]]
      possibleKeys = intersect gridKeys keys
      _makeEntropy :: MonadIO m => Entropy -> (Int, Int) -> WFCSystemT m Entropy
      _makeEntropy acc (x, y) = do
        tiles <- possibleTiles
                 (grid M.!? (x, y-1), grid M.!? (x+1, y), grid M.!? (x, y+1), grid M.!? (x-1, y))
        case V.length tiles of
          0 -> error "no possible tiles"
          1 -> do
            modify global $ \(Grid g) -> Grid $ M.insert (x, y) (tiles V.! 0) g
            return acc
          _ -> return $ M.insert (x, y) (V.length tiles) acc
  foldM _makeEntropy M.empty possibleKeys
