{-# LANGUAGE MultiWayIf #-}

module Main where

-- tigris
import Tigris.ECS.Load.DB

-- text
import Data.Text hiding (null)

-- sqlite-simple
import Database.SQLite.Simple

-- options
import Options

-- vector
import qualified Data.Vector as V

-- base
import System.Exit
import System.IO
import Text.Read
import Control.Monad 

  

main :: IO ()
main = runCommand $ \opts args -> do
  when (not $ null args) (do putStrLn "use '--help' for options"; exitSuccess)
  when (not $ null $ optCreate opts) (do (createDatabase $ optCreate opts); exitSuccess)
  when (not $ null $ optAdd    opts) (do (addTileDBs $ optAdd opts); exitSuccess)
  when (not $ null $ optPrint  opts) (do (printDB $ optPrint opts); exitSuccess)
  putStrLn "use '--help' for option"
  exitSuccess

-- Getline that checks for ":q" to quit program
myGetLine :: IO String
myGetLine = do
  str <- getLine
  if str == ":q" then exitSuccess else return str

-- putStr that flushes always
myPutStr :: String -> IO ()
myPutStr str = do
  putStr str
  hFlush stdout

-- Validates an integer            
validateInt :: String -> Maybe Int
validateInt = readMaybe

intGetLine :: IO Int
intGetLine = do
  str <- myGetLine
  case validateInt str of
    Nothing -> do
      putStrLn "Invalid int."
      intGetLine
    Just a -> return a

textGetLine :: IO Text
textGetLine = pack <$> myGetLine

createDatabase :: String -> IO ()
createDatabase path = useConnection path $ \conn -> execute_ conn createTileDBs

newTileDBs :: IO [TileDB]
newTileDBs = do
  myPutStr "nconn: "
  n <- intGetLine
  myPutStr "econn: "
  e <- intGetLine
  myPutStr "sconn: "
  s <- intGetLine
  myPutStr "wconn: "
  w <- intGetLine
  myPutStr "weight: "
  weight <- intGetLine
  myPutStr "texId: "
  tid <- intGetLine
  myPutStr "hOffset: "
  h <- intGetLine
  myPutStr "vOffset: "
  v <- intGetLine
  myPutStr "frameNum: "
  fn <- intGetLine
  myPutStr "frameWidth: "
  fw <- intGetLine
  myPutStr "frameHeight: "
  fh <- intGetLine
  myPutStr "borderWidth: "
  b <- intGetLine

  let 
    getRot :: String -> IO Int
    getRot str = do
      myPutStr str
      r <- myGetLine
      if | r == "0"  -> return 0
         | r == "1"  -> return 1
         | r == "2"  -> return 2
         | r == "3"  -> return 3
         | otherwise -> getRot str

  r <- getRot "number of rotations 90deg clockwise (0, 1, 2, or 3): "
  case r of
    0 -> 
      return [ TileDB 0 n e s w weight tid h v fn fw fh 0 b ]
    1 -> 
      return [ TileDB 0 n e s w weight tid h v fn fw fh 0 b
             , TileDB 0 w n e s weight tid h v fn fw fh 1 b
             ]
    2 ->
      return [ TileDB 0 n e s w weight tid h v fn fw fh 0 b
             , TileDB 0 w n e s weight tid h v fn fw fh 1 b
             , TileDB 0 s w n e weight tid h v fn fw fh 2 b
             ]
    3 ->
      return [ TileDB 0 n e s w weight tid h v fn fw fh 0 b
             , TileDB 0 w n e s weight tid h v fn fw fh 1 b
             , TileDB 0 s w n e weight tid h v fn fw fh 2 b
             , TileDB 0 e s w n weight tid h v fn fw fh 3 b
             ]
    _ -> error "Invalid number of rotations."
--  let
--    getFlip :: String -> IO Bool
--    getFlip str = do
--      myPutStr str
--      f <- myGetLine
--      if f /= "y" && f /= "n"
--        then getFlip str
--        else if f == "y"
--             then return True
--             else return False
--  fx <- getFlip "flip over x (y/n): "
--  fy <- getFlip "flip over y (y/n): "
--  
--  case (fy, fx) of
--    (True, True)   ->
--      return [ TileDB 0 n e s w weight tid h v fn fw fh
--             , TileDB 0 n w s e weight tid (h+fw) v fn (-fw) fh
--             , TileDB 0 s e n w weight tid h (v-fh) fn fw (-fh)
--             , TileDB 0 s w n e weight tid (h+fw) (v-fh) fn (-fw) (-fh)
--             ]
--    (True, False)  ->
--      return [ TileDB 0 n e s w weight tid h v fn fw fh
--             , TileDB 0 n w s e weight tid (h+fw) v fn (-fw) fh
--             ]
--    (False, True)  -> 
--      return [ TileDB 0 n e s w weight tid h v fn fw fh
--             , TileDB 0 s e n w weight tid h (v-fh) fn fw (-fh)
--             ]
--    (False, False) -> 
--      return [TileDB 0 n e s w weight tid h v fn fw fh]

addTileDBs :: String -> IO ()
addTileDBs path = do
  tiles <- newTileDBs
  useConnection path $ \conn -> executeMany conn insertTileDB tiles

printDB :: String -> IO ()
printDB path = do
  conn <- open path
  rows <- rowToVectorIO conn
  mapM_ print (rows :: V.Vector TileDB)
  close conn
  
  
data MainOptions = MainOptions
  { optCreate :: String
  , optAdd    :: String
  , optPrint  :: String
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "create" ""
        "Create the database."
    <*> simpleOption "add" ""
        "Add an entry to the database."
    <*> simpleOption "print" ""
        "Print the database."

