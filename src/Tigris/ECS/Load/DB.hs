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

-- base
import Data.Typeable (Typeable)


data Entry = Entry
  { id'          :: Int -- ^ DB id 
  , nconnector'  :: Int -- ^ North wfc connector
  , econnector'  :: Int -- ^ East wfc conncect
  , sconnector'  :: Int -- ^ South wfc conncect
  , wconnector'  :: Int -- ^ West wfc conncect
  , weight'      :: Int -- ^ Tile weight
  , texId'       :: Int -- ^ Texture uniform id
  , hOffset'     :: Int -- ^ Horizonal offset in pixels of the top left corner of sprite
  , vOffset'     :: Int -- ^ Vertical offset in pixels of the top left corner of sprite
  , frameNum'    :: Int -- ^ Number of frames in the animation
  , frameWidth'  :: Int -- ^ Width of a frame
  , frameHeight' :: Int -- ^ Height of a frame
  }

instance FromRow Entry where
  fromRow = Entry <$> field
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

instance ToRow Entry where
  toRow (Entry id_ n e s w weight tid h v fn fw fh)
    = toRow $ (Null, n, e, s, w, weight, tid, h, v, fn) :. (fw, fh)


createEntries :: Query
createEntries = [r|
CREATE TABLE IF NOT EXISTS entries
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
  frameHeight INTEGER)
|]

insertEntry :: Query
insertEntry = "INSET INTO entries VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

allEntries :: Query
allEntries = "SELECT * from entries"


useConnection :: (Connection -> IO ()) -> IO ()
useConnection f = do
  putStrLn "Database path: "
  path <- getLine
  conn <- open path
  f conn
  close conn
