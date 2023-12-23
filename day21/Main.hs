{-# LANGUAGE TupleSections #-}

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int, Int)
type Grid = Map Pos Char

coords :: [[Pos]]
coords = map (\y -> map (, y) [0..]) [0..]

adjacent :: Grid -> Pos -> [Pos]
adjacent grid (x,y) =
  filter (`Map.member` grid) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

bfs :: Grid -> Set Pos -> Int -> Set Pos
bfs grid next steps
  | steps > 0 = bfs grid next'' (steps-1)
  | otherwise = next
  where
    next' = foldr (\pos acc -> Set.union acc $ Set.fromList $ adjacent grid pos) Set.empty next
    next'' = Set.filter (\pos -> let c = fromJust $ Map.lookup pos grid in c == '.') next'
    --visited' = Set.union visited next
bfs2 :: Grid -> Set Pos -> Set Pos -> Set Pos
bfs2 grid next visited
  | Set.null next = visited
  | otherwise = bfs2 grid next''' visited'
  where
    next' = foldr (\pos acc -> Set.union acc $ Set.fromList $ adjacent grid pos) Set.empty next
    next'' = Set.filter (\pos -> let c = fromJust $ Map.lookup pos grid in c == '.') next'
    next''' = next'' Set.\\ visited'
    visited' = Set.union visited next

pretty :: Grid -> Set Pos -> IO ()
pretty grid visit = do
  let (x0,y0) = fst $ Map.findMin grid 
  let (x1,y1) = fst $ Map.findMax grid

  mapM_ (\y -> mapM_ (\x -> if Set.member (x,y) visit then putChar 'O' else putChar $ fromJust $ Map.lookup (x,y) grid) [x0..x1] >> putChar '\n') [y0..y1]

vsub :: Pos -> Pos -> Pos
vsub (x0,y0) (x1,y1) = (x0-x1,y0-y1)

distance :: Pos -> Pos -> Int
distance (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

main :: IO ()
main = do
  rows <- lines <$> getContents

  let parsed = concat $ zipWith zip coords rows
  let grid = Map.fromList parsed
  let start = Map.foldrWithKey (\k v p -> if v == 'S' then k else p) (0,0) grid
  let grid' = Map.insert start '.' grid

  -- part 1
  let ans = Set.size $ bfs grid' (Set.fromList [start]) 64
  print ans

  -- part 2
  let steps = 26501365
  -- We reach each of the edges after 65 steps from the center
  let steps' = steps - 65
  let n = steps' `div` 131
  -- In total it takes 131 steps from the center to visit everything
  -- it also takes exactly 131 steps to get from one edge to the next grid
  -- it takes 196 steps to visit everything from each edge of each grid
  
  -- We visit 202300 grids in each direction
  --
  -- Number of gardens we can visit with an even number of steps from an edge: 7796
  -- ... and with an odd number of steps from an edge: 7819
  -- From the center we can visit 7819 squares with an even number of steps and
  -- 7796 with an odd number of steps
  --
  -- 26501365 is odd, so for the first grid we can visit 7796 squares
  -- for the second grid we start at an edge and make 26501365 - 65 = 26501300
  -- steps which is even, so we visit 7819 squares
  --
  -- for the third grid we start at an edge and make 26501300 - 131 = 26501169 steps
  -- which is odd so we visit 7796
  --

  -- Note, not all squares can be visited, because some are walled in
  let reachable = bfs2 grid (Set.fromList [start]) Set.empty

  -- center (grid-local) small diamond spanning from x=0 to x=130 and y=0 to y=130
  let center = Set.filter (\p -> p `distance` (65,65) <= 65) reachable

  -- The corner parts of the grid, not part of the center diamond
  let corners = Set.filter (\p -> p `distance` (65,65) > 65) reachable

  -- individual corners
  let nw = Set.filter (\p -> let (x',y') = p `vsub` (65,65) in x' < 0 && y' < 0) corners
  let ne = Set.filter (\p -> let (x',y') = p `vsub` (65,65) in x' > 0 && y' < 0) corners
  let sw = Set.filter (\p -> let (x',y') = p `vsub` (65,65) in x' < 0 && y' > 0) corners
  let se = Set.filter (\p -> let (x',y') = p `vsub` (65,65) in x' > 0 && y' > 0) corners

  -- Each of the individual pointy ends of the large (global) diamond shape.
  --
  -- When we arrive at the final grids (pointy ends of the global diamond) we start from an edge and we
  -- can only make 131 steps.
  --
  -- The following is functionally equivalent to doing a 131-step BFS from each edge's center. The only
  -- squares we can end up visiting next are odd-numbered squares (x+y is an odd number)
  let point_parity = odd
  let north = Set.filter (\(x,y) -> point_parity (x + y)) $ foldr Set.union Set.empty [center, se, sw]
  let east = Set.filter (\(x,y) -> point_parity (x + y)) $ foldr Set.union Set.empty [center, sw, nw]
  let south = Set.filter (\(x,y) -> point_parity (x + y)) $ foldr Set.union Set.empty [center, ne, nw]
  let west = Set.filter (\(x,y) -> point_parity (x + y)) $ foldr Set.union Set.empty [center, se, ne]


  -- For each diagonal side there are exactly n-1 'big' grid slices and n 'small' grid slices. 
  -- Functionally equivalent to doing a 65-step BFS from each of the grid's corners
  --
  -- It turns out that we can only end up visiting squares which are even-numbered (x+y is even)
  let ne_small = Set.filter (\(x,y) -> (not . point_parity) (x+y)) sw
  let nw_small = Set.filter (\(x,y) -> (not . point_parity) (x+y)) se
  let se_small = Set.filter (\(x,y) -> (not . point_parity) (x+y)) nw
  let sw_small = Set.filter (\(x,y) -> (not . point_parity) (x+y)) ne

  -- Functionally equivalent to doing a 131-step bfs from the center of two (adjacent) edges
  let ne_big = Set.filter (\(x,y) -> point_parity (x+y)) $ foldr Set.union Set.empty [center, se, sw, nw]
  let nw_big = Set.filter (\(x,y) -> point_parity (x+y)) $ foldr Set.union Set.empty [center, se, sw, ne]
  let se_big = Set.filter (\(x,y) -> point_parity (x+y)) $ foldr Set.union Set.empty [center, ne, nw, sw]
  let sw_big = Set.filter (\(x,y) -> point_parity (x+y)) $ foldr Set.union Set.empty [center, ne, nw, se]

  -- Fully explored grid with only the odd-numbered or even-numbered squares marked
  --
  -- Functionally equivalent to doing an even-step, or odd-step BFS that fully explores the grid
  -- (from an even square)
  let full_even = Set.filter (\(x,y) -> even (x+y)) $ foldr Set.union center [ne,nw,se,sw]
  let full_odd = Set.filter (\(x,y) -> odd (x+y)) $ foldr Set.union center [ne,nw,se,sw]

  let ne_side = Set.size ne_small * n + Set.size ne_big * (n-1)
  let nw_side = Set.size nw_small * n + Set.size nw_big * (n-1)
  let se_side = Set.size se_small * n + Set.size se_big * (n-1)
  let sw_side = Set.size sw_small * n + Set.size sw_big * (n-1)

  let sides = ne_side + nw_side + se_side + sw_side
  let points = Set.size north + Set.size east + Set.size south + Set.size west

  -- We can visit all even-numbered squares in all odd-numbered grids that we can fully explore
  let grid_even = 0 + sum (map (* 4) $ filter odd [1..n-1])
  
  -- And vice-versa for all even-numbered grids.
  let grid_odd  = 1 + sum (map (* 4) $ filter even [1..n-1])

  let ans2 = sides + points + grid_even * Set.size full_even + grid_odd * Set.size full_odd

  print ans2
