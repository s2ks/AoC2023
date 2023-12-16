module Main where

import Util
import Data.Maybe
import qualified Data.Map as Map

parseLine :: String -> (String, (String, String))
parseLine line =
  let val = takeWhile (/= ' ') line in
    let rest = (split ',' . drop 2 . dropWhile (/= '=')) line in
      let (left, right) = ((tail . head) rest, (init . drop 1 . last) rest) in
        (val, (left, right))

getValFromDir :: Char -> String -> Map.Map String (String,String) -> String
getValFromDir d node map =
  let (l, r) = fromJust (Map.lookup node map) in
    if d == 'L' then l else r

endsIn :: Char -> String -> Bool
endsIn c [_, _, v] = v == c
endsIn _ _ = False

visits:: String -> String -> Map.Map String (String, String) -> [String]
visits start (d:ds) kvMap =
  start : visits (getValFromDir d start kvMap) ds kvMap

main :: IO ()
main = do
  lines <- input

  let directions = head lines
  let nodes = (map parseLine . drop 2) lines
  let kvMap = foldr (\(k, v) m -> Map.insert k v m) Map.empty nodes

  -- part 1
  let ans1 =
        until
        (\l -> l > 0 && (l `rem` length directions == 0))
        (\l -> (length . takeWhile (/= "ZZZ") . drop l) $ visits "AAA" (cycle directions) kvMap) 
        0
  print ans1

  -- part 2
  let startNodes = (map fst . filter (\(k, _)  -> last k == 'A')) nodes
  let vals
        = length directions
        : map (
            \start -> length . takeWhile (not . endsIn 'Z') $ visits start (cycle directions) kvMap
        ) startNodes

  let ans2 = foldr lcm 1 vals
  print ans2
