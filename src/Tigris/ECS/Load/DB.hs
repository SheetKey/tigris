{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Tigris.ECS.Load.DB where

-- sqlite-simple
import Database.SQLite.Simple
import Database.SQLite.Simple.Types (Null(..))

-- text
import Data.Text

-- raw-strings-qq
import Text.RawString.QQ (r)

-- vector
import qualified Data.Vector as V

-- base
import Data.Typeable (Typeable)


data TileDB = TileDB
  { id'          :: Int -- ^ DB id 
  , nconnector'  :: Int -- ^ North wfc connector
  , econnector'  :: Int -- ^ East wfc conncect
  , sconnector'  :: Int -- ^ South wfc conncect
  , wconnector'  :: Int -- ^ West wfc conncect
  , weight'      :: Int -- ^ TileDB weight
  , texId'       :: Int -- ^ Texture uniform id
  , hOffset'     :: Int -- ^ Horizonal offset in pixels of the top left corner of sprite
  , vOffset'     :: Int -- ^ Vertical offset in pixels of the top left corner of sprite
  , frameNum'    :: Int -- ^ Number of frames in the animation
  , frameWidth'  :: Int -- ^ Width of a frame
  , frameHeight' :: Int -- ^ Height of a frame
  , rotation'    :: Int -- ^ How many times a tile is rotated (0, 1, 2, or 3 90deg rotations)
  , borderWidth' :: Int -- ^ The width of the border around the texture in the texture atlas.
  }
  deriving (Show, Eq)

instance FromRow TileDB where
  fromRow = TileDB <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance ToRow TileDB where
  toRow (TileDB id_ n e s w weight tid h v fn fw fh r b)
    = toRow $ (Null, n, e, s, w, weight, tid, h, v, fn) :. (fw, fh, r, b)


createTileDBs :: Query
createTileDBs = [r|
CREATE TABLE IF NOT EXISTS tiles
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
  nconnector INTEGER,
  econnector INTEGER,
  sconnector INTEGER,
  wconnector INTEGER,
  weight INTEGER,
  texId INTEGER,
  hOffset INTEGER,
  vOffset INTEGER,
  frameNum INTEGER,
  frameWidth INTEGER,
  frameHeight INTEGER,
  rotation INTEGER,
  borderWidth INTEGER)
|]

insertTileDB :: Query
insertTileDB = "INSERT INTO tiles VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

allTileDBs :: Query
allTileDBs = "SELECT * from tiles"

_getTileFromId :: Query
_getTileFromId = "SELECT * from tiles where id = ?"

getTileFromId :: Int -> Connection -> IO TileDB
getTileFromId i conn = do
  res <- query conn _getTileFromId (Only i)
  case res of
    [x] -> return x
    _   -> error "Invalid tiledb id."
  

useConnection :: String -> (Connection -> IO ()) -> IO ()
useConnection path f = do
  conn <- open path
  f conn
  close conn

rowToVector :: V.Vector TileDB -> TileDB -> IO (V.Vector TileDB)
rowToVector v t = return $ V.cons t v

rowToVectorIO :: Connection -> IO (V.Vector TileDB)
rowToVectorIO conn = fold_ conn allTileDBs V.empty rowToVector
