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
import Control.Exception (throw
                         , AssertionFailed (AssertionFailed)
                         )

-- random
import System.Random (getStdGen)

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
  deriving (Eq)


type Entropy = M.Map (Int, Int) Int

newtype AllTiles = AllTiles (V.Vector Tile) deriving (Semigroup, Monoid)
instance Component AllTiles where
  type Storage AllTiles = ReadOnly (Global AllTiles)

newtype Grid = Grid (M.Map (Int, Int) Tile) deriving (Semigroup, Monoid)
instance Component Grid where
  type Storage Grid = Global Grid

newtype MapSize = MapSize (Int, Int) 
instance Semigroup MapSize where
  _ <> _ = error "'<>' should not be used for 'MapSize'."
instance Monoid MapSize where
  mempty = MapSize (0, 0)
instance Component MapSize where
  type Storage MapSize = Global MapSize

newtype TileAdjacencyN = TileAdjacencyN (IM.IntMap (V.Vector Tile)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyN where
  type Storage TileAdjacencyN = ReadOnly (Global TileAdjacencyN)

newtype TileAdjacencyE = TileAdjacencyE (IM.IntMap (V.Vector Tile)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyE where
  type Storage TileAdjacencyE = ReadOnly (Global TileAdjacencyE)

newtype TileAdjacencyS = TileAdjacencyS (IM.IntMap (V.Vector Tile)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyS where
  type Storage TileAdjacencyS = ReadOnly (Global TileAdjacencyS)

newtype TileAdjacencyW = TileAdjacencyW (IM.IntMap (V.Vector Tile)) deriving (Semigroup, Monoid)
instance Component TileAdjacencyW where
  type Storage TileAdjacencyW = ReadOnly (Global TileAdjacencyW)


makeWorld "WFCWorld" [ ''Grid
                     , ''MapSize
                     , ''AllTiles
                     , ''TileAdjacencyN
                     , ''TileAdjacencyE
                     , ''TileAdjacencyS
                     , ''TileAdjacencyW
                     ]

type WFCSystemT m a = SystemT WFCWorld m a


possibleTiles :: MonadIO m => (Int, Int) -> WFCSystemT m (V.Vector Tile)
possibleTiles (x, y) = do
  Grid grid <- get global
  _possibleTiles (grid M.!? (x, y-1), grid M.!? (x+1, y), grid M.!? (x, y+1), grid M.!? (x-1, y))
  where 
    -- inputs neightboring tiles
    _possibleTiles :: MonadIO m
                  => (Maybe Tile, Maybe Tile, Maybe Tile, Maybe Tile)
                  -> WFCSystemT m (V.Vector Tile)
    _possibleTiles (n, e, s, w) = do
      let
        -- Get the corresponding connector from neighboring tiles if there are
        -- neighboring tiles in the grid
        connectN = case n of
                     Nothing  -> Nothing
                     Just ntile -> Just $ sconnector ntile
        connectE = case e of
                     Nothing  -> Nothing
                     Just etile -> Just $ wconnector etile
        connectS = case s of
                     Nothing  -> Nothing
                     Just stile -> Just $ nconnector stile
        connectW = case w of
                     Nothing  -> Nothing
                     Just wtile -> Just $ econnector wtile
      AllTiles vtiles <- get global
      let
        -- Get the tiles with the correct n,e,s,w connector if there is
        -- a required n,e,s,w connector.
        -- If there is not a required connection, return all tiles.
        _possibleN = case connectN of
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
      -- return the tiles that have the write n,e,s,w connector.
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


makeEntropy :: MonadIO m => WFCSystemT m Entropy
makeEntropy = do
  MapSize (xMax, yMax) <- get global
  Grid grid <- get global
  let
    -- all of the cells that have been collapsed
    gridKeys = M.keys grid
    -- all the possible cells
    -- TODO: store this as a global read only variable for efficiency
    keys = [(x, y) | x <- [0..xMax], y <- [0..yMax]]
    -- the cells that have not yet been collapsed
    possibleKeys = intersect gridKeys keys
    -- a function used to fold over the possibleKeys
    _makeEntropy :: MonadIO m => Entropy -> (Int, Int) -> WFCSystemT m Entropy
    _makeEntropy acc cell = do
      -- get the possible tiles in the cell
      tiles <- possibleTiles cell
      case V.length tiles of
        -- no possible tiles
        -- TODO: this should probably use 'throw'
        0 -> error "no possible tiles"
        -- If there are possible tiles, add the number of tiles to the entropy.
        _ -> return $ M.insert cell (V.length tiles) acc
  -- fold over the possbleKeys to get a map of the entropy
  foldM _makeEntropy M.empty possibleKeys


leastEntropy :: Entropy -> Maybe (Int, Int)
leastEntropy = (fmap fst) . M.foldrWithKey
               (\k a b -> case a of
                            0 -> throw $ AssertionFailed "Zero entropy found."
                            _ -> case b of
                                   Nothing       -> Just (k, a)
                                   Just (_, be) -> case compare a be of
                                                     GT -> b
                                                     EQ -> Just (k, a)
                                                     LT -> Just (k, a)
               )
               Nothing
    
weightedChoice :: MonadIO m => (Int, Int) -> WFCSystemT m Tile
weightedChoice cell = do
  tiles <- possibleTiles cell
  let weightList :: [(Double, Tile)]
      weightList = [ (fromIntegral $ weight tile, tile) | tile <- V.toList tiles ]
      tileRVar = weightedCategorical weightList
  thing <- fmap (runState (sampleStateRVar tileRVar)) getStdGen
  return $ fst thing
