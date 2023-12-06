module Main where

import Util


-- >>> product $ countWays [7, 15, 30] [9, 40, 200] []
-- 288

countWays :: [Int] -> [Int] -> [Int] -> [Int]
countWays [] [] acc = acc
countWays (t:times) (d:distances) acc =
   countWays times distances $ (length . filter (> d) . map (\hold -> (t - hold) * hold)) [1..t] : acc
  

main :: IO ()
main = do
  lines <- input

  let times = (map read . words . drop 1 . dropWhile (/= ':') . head) lines
  let distances = (map read . words . drop 1 . dropWhile (/= ':') . last) lines

  let ways = countWays times distances []

  print $ product ways


  let time = (read . concat  . words . drop 1 . dropWhile (/= ':') . head) lines :: Int 
  let distance = (read. concat . words . drop 1 . dropWhile (/= ':') . last) lines :: Int

  print $ product (countWays [time] [distance] [])
