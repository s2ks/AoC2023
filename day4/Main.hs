module Main where

import Util

-- >>> 5 `elem` [1,3,5,66,7]
-- True

score :: [String] -> Int
score (card:have:_) =
  let card' = map read $ words card :: [Int]
      have' = map read $ words have :: [Int]
  in
    foldr (
      \n acc ->
        if n `elem` have' then
          if acc == 0 then
            1
          else
            acc * 2
        else
          acc
    ) 0 card'

type Card = (Int, ([Int], [Int]))

test = [(0,([41,48,83,86,17],[83,86,6,31,17,9,48,53])),(1,([13,32,20,16,61],[61,30,68,82,17,32,24,19])),(2,([1,21,53,59,44],[69,82,63,72,16,21,14,1])),(3,([41,92,73,84,69],[59,84,76,51,58,5,54,83])),(4,([87,83,26,28,32],[88,30,70,12,93,22,82,36])),(5,([31,18,13,56,72],[74,77,10,23,35,67,36,11]))] :: [Card]

-- >>> process test test 0
-- 30

process :: [Card] -> [Card] -> Int -> Int
process all [] count = count
process all (cur:cards) count =
  let (i, (win, have)) = cur
      addCopies = foldr (\n acc -> acc + if n `elem` have then 1 else 0) 0 win
  in
   let next = map (\d -> all !! (i + d)) [1..addCopies] in
      process all (next ++ cards) (count + 1)

main :: IO ()
main = do
  lines <- input

  let stripped = map (split '|' . drop 1 . dropWhile (/= ':')) lines
  let grouped = map (\(card:have:_) -> (map read $ words card, map read $ words have)) stripped :: [([Int], [Int])]

  let cards = zip [0..] grouped

  -- part 1
  let scores = map score stripped
  let ans1 = sum scores
  print ans1

  -- part 2
  let ans2 = process cards cards 0
  print ans2
