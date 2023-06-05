{-# LANGUAGE RecordWildCards #-}

module Tigris.ECS.Load.TileMap
  ( loadNewGrid
  ) where

-- tigris
import Tigris.ECS.Load.DB
import Tigris.WFC
import Tigris.ECS.System
import Tigris.ECS.Components
import Tigris.ECS.Process.UV
import Paths_tigris

-- sqlite-simple
import Database.SQLite.Simple

-- containers
import qualified Data.Map.Strict as M

-- base
import Control.Monad.IO.Class

-- apecs
import Apecs

-- linear
import Linear

-- vector
import qualified Data.Vector as V

tileDBtoTile :: TileDB -> Tile
tileDBtoTile (TileDB _id n e s w weight _ _ _ _ _ _ _ _) = Tile _id n e s w weight

genNewGrid :: String -> Int -> (Tile -> (Int, Int) -> V.Vector Tile -> V.Vector Tile) -> IO Grid
genNewGrid path radius f = do
  conn <- open path
  tiledbs <- rowToVectorIO conn
  close conn
  let tiles = tileDBtoTile <$> tiledbs
  wfc tiles (16, 16) Nothing radius f

tileToEnt :: MonadIO m => (Int, Int) -> TileDB -> SystemT' m ()
tileToEnt (x, y) TileDB {..} = 
  let
    p = fromIntegral <$> (V3 (64 * x) (-1) (64 * (-y)))
    pos = Position (V4 p p p p)
    _uv = mkUV hOffset' vOffset' frameWidth' frameHeight' borderWidth'
  in case rotation' of
    0 -> newEntity_ ( _uv, pos, Size (V4 (V3 0 0 (-64)) (V3 64 0 (-64)) (V3 64 0 0) (V3 0 0 0)) )
    1 -> newEntity_ ( _uv, pos, Size (V4 (V3 64 0 (-64)) (V3 64 0 0) (V3 0 0 0) (V3 0 0 (-64))) )
    2 -> newEntity_ ( _uv, pos, Size (V4 (V3 64 0 0) (V3 0 0 0) (V3 0 0 (-64)) (V3 64 0 (-64))) )
    3 -> newEntity_ ( _uv, pos, Size (V4 (V3 0 0 0) (V3 0 0 (-64)) (V3 64 0 (-64)) (V3 64 0 0)) )
    _ -> error "Wrong number of rotations found in database."
  

loadGrid :: MonadIO m => String -> Grid -> SystemT' m ()
loadGrid path (Grid g) = do
  conn <- liftIO $ open path
  let
    grid = M.toList $ tileId <$> g
    mkEnt :: MonadIO m => ((Int, Int), Int) -> SystemT' m ()
    mkEnt (pos, _id) = do
      tiledb <- liftIO $ getTileFromId _id conn
      tileToEnt pos tiledb
  mapM_ mkEnt grid
  liftIO $ close conn

loadNewGrid :: MonadIO m => String -> Int -> (Tile -> (Int, Int) -> V.Vector Tile -> V.Vector Tile) -> SystemT' m ()
loadNewGrid p radius f= do
  path <- liftIO $ getDataFileName p
  grid <- liftIO $ genNewGrid path radius f
  loadGrid path grid
