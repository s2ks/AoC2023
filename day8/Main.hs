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

traverseWhile :: (String -> Bool) -> String -> String -> Map.Map String (String,String) -> Int
traverseWhile p ds node kvMap = countSteps node ds 0
  where
    countSteps cur (d:ds') acc =
      let next = getValFromDir d cur kvMap in
        if p next then
          countSteps next ds' (acc + 1)
        else
          acc + 1
        
endsInZ :: String -> Bool
endsInZ [_, _, c] = c == 'Z'
endsInZ _ = False

main :: IO ()
main = do
  lines <- input

  let directions = head lines
  let nodes = (map parseLine . drop 2) lines
  let kvMap = foldr (\(k, v) m -> Map.insert k v m) Map.empty nodes

  -- part 1
  let ans1 = traverseWhile (/= "ZZZ") (cycle directions) "AAA" kvMap
  print ans1

  -- part 2
  let startNodes = (map fst . filter (\(k, _)  -> last k == 'A')) nodes
  let vals = map (\node -> traverseWhile (not . endsInZ) (cycle directions) node kvMap) startNodes
  let ans2 = foldr lcm 1 vals
  print ans2
