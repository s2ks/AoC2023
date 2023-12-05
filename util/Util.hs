module Util where

import System.IO
import Control.Concurrent

input :: IO [String]
input = input' []
  where
    input' acc = do
      done <- isEOF
      if done then
        return $ reverse acc
      else do
        line <- getLine
        input' (line:acc)

split :: Char -> String -> [String]
split c s = split' c s []
  where
    split' c [] acc = reverse acc
    split' c s acc =
      let acc' = takeWhile (/= c) s : acc
      in split' c ((drop 1 . dropWhile (/= c)) s) acc'

forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n work = do
  -- Fork the threads and create a list of the MVars which
  -- per thread tell whether the work has finished.
  finishVars <- mapM work' [0 .. n - 1]
  -- Wait on all MVars
  mapM_ takeMVar finishVars
  where
    work' :: Int -> IO (MVar ())
    work' index = do
      var <- newEmptyMVar
      _   <- forkOn index (work index >> putMVar var ())
      return var
