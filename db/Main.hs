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
  when (not $ null $ optAdd    opts) (do (addEntries $ optAdd opts); exitSuccess)
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
createDatabase path = useConnection path $ \conn -> execute_ conn createEntries

newEntries :: IO [Entry]
newEntries = do
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

  let
    getFlip :: String -> IO Bool
    getFlip str = do
      myPutStr str
      f <- myGetLine
      if f /= "y" && f /= "n"
        then getFlip str
        else if f == "y"
             then return True
             else return False
  fx <- getFlip "flip over x (y/n): "
  fy <- getFlip "flip over y (y/n): "
  
  case (fx, fy) of
    (True, True)   ->
      return [ Entry 0 n e s w weight tid h v fn fw fh
             , Entry 0 n w s e weight tid (h+fw) v fn (-fw) fh
             , Entry 0 s e n w weight tid h (v-fh) fn fw (-fh)
             , Entry 0 s w n e weight tid (h+fw) (v-fh) fn (-fw) (-fh)
             ]
    (True, False)  ->
      return [ Entry 0 n e s w weight tid h v fn fw fh
             , Entry 0 n w s e weight tid (h+fw) v fn (-fw) fh
             ]
    (False, True)  -> 
      return [ Entry 0 n e s w weight tid h v fn fw fh
             , Entry 0 s e n w weight tid h (v-fh) fn fw (-fh)
             ]
    (False, False) -> 
      return [Entry 0 n e s w weight tid h v fn fw fh]

addEntries :: String -> IO ()
addEntries path = do
  entries <- newEntries
  useConnection path $ \conn -> executeMany conn insertEntry entries

rowToVector :: V.Vector Entry -> Entry -> IO (V.Vector Entry)
rowToVector v t = return $ V.cons t v

rowToVectorIO :: Connection -> IO (V.Vector Entry)
rowToVectorIO conn = fold_ conn allEntries V.empty rowToVector

printDB :: String -> IO ()
printDB path = do
  conn <- open path
  rows <- rowToVectorIO conn
  mapM_ print (rows :: V.Vector Entry)
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

