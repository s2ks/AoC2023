module Main where

import Util
import Data.List
import Control.Concurrent
import Control.Monad
import Data.IORef

type Range = (Int, Int)
-- ([Dest], [Source])
type Transformer = (Range, Range)
type TransformMap = [Transformer]

transform :: [TransformMap] -> Int -> Int
transform [] src = src
transform (t:ts) src =
  let validTransformers = filter (\(_, (lo, hi)) -> src >= lo && src <= hi) t in
    if null validTransformers then
      transform ts src
    else
      let ((dlo, _), (slo, _)) = head validTransformers in
        let next = dlo + (src - slo) in
          transform ts next

spread :: Int -> Range -> [Range]
spread parts (lo, hi) =
  let range = hi - lo
      partSize = range `div` parts
      leftovers = range - (partSize * parts)
  in
    mkparts lo partSize leftovers parts
  where
    mkparts :: Int -> Int -> Int -> Int -> [Range]
    mkparts lower partSize leftovers n
      | n > 0 = let upper = lower + partSize + if leftovers > 0 then 1 else 0
        in (lower, upper) : mkparts upper partSize (leftovers - 1) (n - 1)
      | otherwise = []


worker :: IORef Int -> IORef Int -> Chan Range -> [TransformMap] -> Int -> IO ()
worker counter ans chan transformers index = do
  count <- readIORef counter

  when (count > 0) $ do
    (lo, hi) <- do
      v <- readChan chan
      let (lo, hi) = v

      if hi-lo > 100000 then do
        writeChan chan (lo+100001, hi)
        return (lo, lo+100000)
      else do
        atomicModifyIORef counter (\old -> (old-1, ()))
        return (lo, hi)

    --print (lo, hi)
    let locations' = map (transform transformers) [lo..hi]

    let min = minimum locations'

    runningMin <- atomicModifyIORef ans (\old -> if old < 0 || min < old then (min, min) else (old, old))

    print (show runningMin <> " " <> show min)
    worker counter ans chan transformers index

main :: IO ()
main = do
  lines <- input

  let seedToSoil = (takeWhile (/= "") . drop 1 . dropWhile (/= "seed-to-soil map:")) lines
  let soilToFert = (takeWhile (/= "") . drop 1 . dropWhile (/= "soil-to-fertilizer map:")) lines
  let fertToWater = (takeWhile (/= "") . drop 1 . dropWhile (/= "fertilizer-to-water map:")) lines
  let waterToLight = (takeWhile (/= "") . drop 1 . dropWhile (/= "water-to-light map:")) lines
  let lightToTemp = (takeWhile (/= "") . drop 1 . dropWhile (/= "light-to-temperature map:")) lines
  let tempToHum = (takeWhile (/= "") . drop 1 . dropWhile (/= "temperature-to-humidity map:")) lines
  let humToLoc = (drop 1 . dropWhile (/= "humidity-to-location map:")) lines


  let toList = [seedToSoil, soilToFert, fertToWater, waterToLight, lightToTemp, tempToHum, humToLoc]
  let phase1 = map (map words) toList
  let phase2 = map (map (map read)) phase1 :: [[[Int]]]
  let transformers =
        map (
          map (
            \(dest:source:range:_)-> ((dest, dest+range), (source, source+range))
          )
        ) phase2 :: [TransformMap]
  
  -- part 1
  let seeds = map read ((words . drop 2 . dropWhile (/= ':')) $ head lines) :: [Int]
  let locations = map (transform transformers) seeds
  let ans1 = minimum locations
  print ans1

  -- part 2
  let genSeeds [] = []
      genSeeds (s:r:xs) = (s,s+r-1) : genSeeds xs
  let seeds2 = genSeeds seeds

  chan <- newChan
  writeList2Chan chan seeds2
  counter <- newIORef $ length seeds2

  ans2 <- newIORef (-1) 
  forkThreads 10 $ worker counter ans2 chan transformers
  res <- readIORef ans2
  print res


