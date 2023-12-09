module Main where

import Util

diffs :: [Int] -> [Int]
diffs (x:y:xs) = y-x:diffs (y:xs)
diffs _ = []

main :: IO ()
main = do
  lines <- input

  let parsed = map (map read . words) lines
  let dsss = map (\xs -> until (all (== 0) . head) (\xss -> (diffs . head) xss : xss) [diffs xs, xs]) parsed

  -- part 1
  let adds = map (foldl (\a ds -> last ds + a) 0) dsss
  print $ sum adds

  -- part 2
  let pres = map (foldl (\a ds -> head ds - a) 0) dsss
  print $ sum pres
