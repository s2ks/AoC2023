module Util where

import Control.Concurrent

input :: IO [String]
input = lines <$> getContents

split :: (Eq a) => a -> [a] -> [[a]]
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

replace :: (Eq a) => a -> a -> [a] -> [a]
replace orig with xs = replace' orig with xs []
  where
    replace' :: Eq a => a -> a -> [a] -> [a] -> [a]
    replace' _ _ [] acc = reverse acc
    replace' v' c' (x:xs') acc = 
      if v' == x then
        replace' v' c' xs' (c':acc)
      else
        replace' v' c' xs' (x:acc)

