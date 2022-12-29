module Main where

import Tigris.ECS.Load.DB

main :: IO ()
main = putStrLn "Hello"

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

createDatabase :: IO ()
createDatabase = useConnection $ \conn -> execute_ conn createEntries

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
    getFlip :: Str -> IO Bool
    getFlip str = do
      myPutStr str
      f <- myGetLine
      if fx /= "y" && fx /= "n"
        then getFlip str
        else if fx == "y"
             then True
             else False
  fx <- getFlip "flip over x: "
  fy <- getFlip "flip over y: "
  
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

addEntries :: IO ()
addEntries = do
  entries <- newEntries
  useConnection $ \conn -> executeMany conn insertEntry entries
  
  
