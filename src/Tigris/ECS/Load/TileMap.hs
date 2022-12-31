module Tigris.ECS.Load.TileMap
  ( loadNewGrid
  ) where

-- tigris
import Tigris.ECS.Load.DB
import Tigris.WFC
import Tigris.ECS.System
import Tigris.ECS.Components
import Paths_tigris

-- sqlite-simple
import Database.SQLite.Simple

-- vector
import qualified Data.Vector as V

-- containers
import qualified Data.Map.Strict as M

-- base
import Control.Monad.IO.Class

-- apecs
import Apecs

-- linear
import Linear

tileDBtoTile :: TileDB -> Tile
tileDBtoTile (TileDB id n e s w weight _ _ _ _ _ _ _) = Tile id n e s w weight

genNewGrid :: String -> IO Grid
genNewGrid path = do
  conn <- open path
  tiledbs <- rowToVectorIO conn
  close conn
  let tiles = tileDBtoTile <$> tiledbs
  wfc tiles (16, 16) Nothing


loadGrid :: MonadIO m => String -> Grid -> SystemT' m ()
loadGrid path (Grid g) = do
  conn <- liftIO $ open path
  let
    grid = M.toList $ tileId <$> g
    tileToEnt :: MonadIO m => ((Int, Int), Int) -> SystemT' m ()
    tileToEnt ((x, y), id) = do
      tiledb <- liftIO $  getTileFromId id conn
      let p = fromIntegral <$> (V3 (64*x) 0 (64*(-y)))
          pos = Position (V4 p p p p)
          f = V2 (/ fromIntegral sheetWidth) (/ fromIntegral sheetHeight)
          uv = UV (fmap (f <*>)
                       ((fmap . fmap) fromIntegral
                        (V4 (V2 (hOffset' tiledb) (vOffset' tiledb - frameHeight' tiledb))                      -- bl
                            (V2 (hOffset' tiledb + frameWidth' tiledb) (vOffset' tiledb - frameHeight' tiledb)) -- br
                            (V2 (hOffset' tiledb + frameWidth' tiledb) (vOffset' tiledb))                       -- tr
                            (V2 (hOffset' tiledb) (vOffset' tiledb))                                            -- tl
                        )))
      case rotation' tiledb of
        0 -> newEntity_ ( uv, pos, Size (V4 (V3 0 0 0) (V3 64 0 0) (V3 64 0 (-64)) (V3 0 0 (-64))) )
        1 -> newEntity_ ( uv, pos, Size (V4 (V3 0 0 (-64)) (V3 0 0 0) (V3 64 0 0) (V3 64 0 (-64))) )
        2 -> newEntity_ ( uv, pos, Size (V4 (V3 64 0 (-64)) (V3 0 0 (-64)) (V3 0 0 0) (V3 64 0 0)) )
        3 -> newEntity_ ( uv, pos, Size (V4 (V3 64 0 0) (V3 64 0 (-64)) (V3 0 0 (-64)) (V3 0 0 0)) )
        _ -> error "Wrong number of rotations found in database."
  mapM_ tileToEnt grid
  liftIO $ close conn

loadNewGrid :: MonadIO m => String -> SystemT' m ()
loadNewGrid p = do
  path <- liftIO $ getDataFileName p
  grid <- liftIO $ genNewGrid path
  liftIO $ print grid
  loadGrid path grid
