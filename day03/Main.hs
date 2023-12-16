module Main where

import Data.Char
import Data.List
import Data.Bifunctor
import Util

dots :: String
dots = '.' : dots

type Number = (Int, [(Char, (Int, Int))])

traverseLine :: String -> String -> String -> Int -> [Number]
traverseLine prev next cur row =
  let width = length cur in
    let (_, _, res) = foldl (\(tally, adj, acc) (i, c) ->
          let (u, d) = ((prev !! i, (row - 1, i)), (next !! i, (row + 1, i))) in
            if isDigit c && i == (width - 1) then
              ("", [], (read $ reverse (c:tally), u:d:adj):acc)
            else if (not . isDigit) c && (not . null) tally then
              ("", [(c, (row, i)), u, d], (read $ reverse tally, (c, (row, i)):u:d:adj):acc)
            else if isDigit c then
              (c:tally, u:d:adj, acc)
            else
              ("", [(c, (row, i)), u, d], acc)
          ) ("", [], []) $ zip [0..] cur in
            let filtered = map (second $ filter (\(c, _) -> c /= '.')) res in
              filter (\(_, adj) -> (not . null) adj) filtered

traverseInput :: [String] -> [[Number]]
traverseInput input = let height = length input in
  zipWith (\i line ->
    let (prevRow, nextRow) = (
          if i - 1 < 0 then dots else input !! (i-1),
          if i + 1 >= height then dots else input !! (i+1))
    in traverseLine prevRow nextRow line i) [0..] input

main :: IO ()
main = do
  lines <- input

  -- part 1
  let nums = (map fst . concat . traverseInput) lines
  print $ sum nums

  -- part 2
  let nums2 = (filter (not . null . snd)
              . map (second $ filter (\(c, _) -> c == '*'))
              . concat
              . traverseInput) lines

  let duped = foldr (
        \(n, adj) acc -> 
          map (\(_, (x, y)) -> (n, (x, y))) adj ++ acc
        ) [] nums2

  let sorted = sortBy (\(_, a) (_, b) -> a `compare` b) duped
  let grouped = (filter (\e -> length e == 2) . map (map fst) . groupBy (\(_, a) (_, b) -> a == b)) sorted 

  let ans2 = map (\(a:b:_) -> a * b) grouped

  print $ sum ans2
