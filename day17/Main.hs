{-# LANGUAGE TupleSections #-}

import Algorithm.Search (dijkstra)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Direction = Start | U | D | L | R deriving (Show, Eq, Ord)
type Pos = (Int, Int)
type Node = (Direction, Int, Pos)

coords :: [[Pos]]
coords = map (\y -> map (, y) [0..]) [0..]

neighbours :: Map Pos Int -> Node -> [Node]
neighbours grid (d, s, (x, y))
  | d == U = filt [(R, 1, (x+1, y)), (L, 1, (x-1, y)), (U, s+1, (x, y-1))]
  | d == D = filt [(R, 1, (x+1, y)), (L, 1, (x-1, y)), (D, s+1, (x, y+1))]
  | d == L = filt [(L, s+1, (x-1, y)), (D, 1, (x, y+1)), (U, 1, (x, y-1))]
  | d == R = filt [(R, s+1, (x+1, y)), (D, 1, (x, y+1)), (U, 1, (x, y-1))]
  | otherwise = filt [(L, 1, (x-1, y)), (R, 1, (x+1, y)), (D, 1, (x, y+1)), (U, 1, (x, y-1))]
  where filt = filter (\(_, s', p) -> p `Map.member` grid && s' <= 3)

neighbours2 :: Map Pos Int -> Node -> [Node]
neighbours2 grid (d, s, (x, y))
  | d == U = filt $ (U, s+1, (x, y-1)) : if s >= 4 then [(R, 1, (x+1, y)), (L, 1, (x-1, y))] else []
  | d == D = filt $ (D, s+1, (x, y+1)) : if s >= 4 then [(R, 1, (x+1, y)), (L, 1, (x-1, y))] else []
  | d == L = filt $ (L, s+1, (x-1, y)) : if s >= 4 then [(D, 1, (x, y+1)), (U, 1, (x, y-1))] else []
  | d == R = filt $ (R, s+1, (x+1, y)) : if s >= 4 then [(D, 1, (x, y+1)), (U, 1, (x, y-1))] else []
  | otherwise = filt [(L, 1, (x-1, y)), (R, 1, (x+1, y)), (D, 1, (x, y+1)), (U, 1, (x, y-1))]
  where filt = filter (\(_, s', p) -> p `Map.member` grid && s' <= 10)

edgeCost :: Map Pos Int -> Node -> Node -> Int
edgeCost grid _ (_,_,p)  = fromJust $ Map.lookup p grid

main :: IO ()
main = do
  lns <- lines <$> getContents

  let rows = zipWith zip coords lns
  let parsed = concatMap (map (\(xy, c) -> (xy, read [c]))) rows
  let grid = Map.fromList parsed
  let dest = maximum $ map fst parsed

  let (cost, _) =
        fromJust $ dijkstra
        (neighbours grid)
        (edgeCost grid)
        (\(_,_,p) -> p == dest)
        (Start, 0, (0,0))
  print cost

  let (cost2, _) =
        fromJust $ dijkstra
        (neighbours2 grid)
        (edgeCost grid)
        (\(_,_,p) -> p == dest)
        (Start, 0, (0,0))
  print cost2
