import Data.Map (Map, (!))
import Data.Set (Set)

import Data.List
import Data.Ord

import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int, Int)
type Grid = Map Pos Char
type Graph = Map Pos (Set Pos)

type WeightedGraph = Map Pos (Set Node)
type Node = (Int, Pos)

coords :: [[Pos]]
coords = map (\y -> map (, y) [0..]) [0..]

adjacent :: Grid -> Pos -> [Pos]
adjacent grid (x,y)
  | cell == 'v' = [(x,y+1)]
  | cell == '>' = [(x+1,y)]
  | cell == '<' = [(x-1,y)]
  | cell == '^' = [(x,y-1)]
  | otherwise = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
  where
    cell = grid ! (x,y)

adjacentF :: Grid -> Pos -> [Pos]
adjacentF grid pos = filter (`Map.member` grid) $ adjacent grid pos

longestPath :: WeightedGraph -> Set Pos -> Int -> Pos -> Pos  -> Int
longestPath junctions visited steps dest pos
  | pos == dest = steps
  | null vsteps = 0
  | otherwise = maximum vsteps
    where
    visited' = Set.insert pos visited

    vsteps = 
      map (\(weight,p) -> longestPath junctions visited' (steps + weight) dest p) $
      Set.toList adjacentNodes

    adjacentNodes :: Set Node
    adjacentNodes = Set.filter (\(_, p) -> p `Set.notMember` visited) (junctions ! pos)

buildGraph :: Grid -> Graph
buildGraph grid = Map.mapWithKey (\k _ -> Set.fromList $ adjacentF grid k) grid


getJunctions :: Pos -> Pos -> Graph -> Graph
getJunctions src dest graph =
  Map.insert src (graph ! src) $
  Map.insert dest (graph ! dest) $
  Map.filter ((> 2) . Set.size) graph

buildJunctionGraph :: Graph -> Graph -> Pos -> Set Node
buildJunctionGraph juncts graph src = travelToNextJunction 0 Set.empty src
  where
    isJunction pos = pos `Map.member` juncts
    travelToNextJunction steps visited pos
      | isJunction pos && pos /= src = Set.singleton (steps, pos)
      | otherwise =
        let visited' = Set.insert pos visited in
          foldl' (\acc p -> acc `Set.union` travelToNextJunction (steps+1) visited' p) Set.empty $
          (graph ! pos) Set.\\ visited
main :: IO ()
main = do
  rows <- lines <$> getContents

  let cells = filter ((/= '#') . snd) $ concat $ zipWith zip coords rows
  let grid = Map.fromList cells
  let (src, dest) = (fst $ minimumBy (comparing fst) cells, fst $ maximumBy (comparing fst) cells)

  -- part 1
  let graph = buildGraph grid
  let juncts = getJunctions src dest graph
  let junctionGraph = Map.mapWithKey (\k _ -> buildJunctionGraph juncts graph k) juncts
  let ans = longestPath junctionGraph Set.empty 0 dest src
  print ans

  -- part 2
  let grid2 = Map.map (const '.') grid
  let graph2 = buildGraph grid2
  let juncts2 = getJunctions src dest graph
  let junctionGraph2 = Map.mapWithKey (\k _ -> buildJunctionGraph juncts2 graph2 k) juncts2
  let ans2 = longestPath junctionGraph2 Set.empty 0 dest src
  print ans2
