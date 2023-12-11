{-# LANGUAGE TupleSections #-}

import Data.List
import qualified Data.Set as Set

type Pos = (Int, Int)
type Cell = (Char, Pos)

coords :: [[Pos]]
coords = map (\y -> map (, y) [0..]) [0..]

findStart :: [[Cell]] -> Pos
findStart (cells:rows) =
  let maybeCell = 'S' `elemIndex` map fst cells in
    case maybeCell of
      Just i -> let (_, pos) = cells !! i in pos
      _ -> findStart rows

cellAt :: [[Cell]] -> Pos -> Cell
cellAt rows (x, y) =
  if x < 0 || y < 0 || x >= length (head rows) || y >= length rows then
    ('.', (-1, -1))
  else
    (rows !! y) !! x

convert :: Char -> Char
convert '|' = '│'
convert '-' = '─'
convert 'L' = '└'
convert 'J' = '┘'
convert 'F' = '┌'
convert '7' = '┐'
convert 'S' = '┼'
convert c = c

getAdjacent :: [[Cell]] -> Cell -> [Cell]
getAdjacent rows (c, (x, y)) = adjacent
  where
    north = (x, y-1)
    east  = (x+1, y)
    south = (x, y+1)
    west  = (x-1, y)
    lookAt =
      case c of
        '|' -> [north, south]
        '-' -> [east, west]
        'L' -> [north, east]
        'J' -> [north, west]
        '7' -> [south, west]
        'F' -> [south, east]
        '.' -> []
        'S' -> [north, east, south, west]
    adjacent = map (cellAt rows) lookAt

findConnected :: Cell -> [[Cell]] -> [Cell]
findConnected thisCell@(c, (x, y)) rows =
  filter (\adj -> thisCell `elem` getAdjacent rows adj) (getAdjacent rows thisCell)

traversePipes :: Cell -> [[Cell]] -> [(Int, Cell)]
traversePipes start rows = doTraverse [start] 0 (Set.singleton start) [(0, start)]
  where
    doTraverse :: [Cell] -> Int -> Set.Set Cell -> [(Int, Cell)] -> [(Int, Cell)]
    doTraverse front steps visited acc =
      let nextFront =
            (map head . group . sort)
            (concatMap (filter (`Set.notMember` visited) . (`findConnected` rows)) front) 
      in
        if null nextFront then
          acc
        else
          let nextVisited = foldr Set.insert visited front in
            let nextAcc = map (steps + 1,) nextFront ++ acc in
              doTraverse nextFront (steps + 1) nextVisited nextAcc

resolveSource :: [[Cell]] -> Cell -> [[Cell]]
resolveSource rows source =
  let conn = findConnected source rows in
    let [(sx, sy), (x1, y1), (x2, y2)] = map snd (source: sort conn) in
      if x1 < sx && x2 > sx then
        map (map (\cell@(c, pos) -> if c == 'S' then ('-', pos) else cell)) rows
      else if y1 < sy && y2 > sy then
        map (map (\cell@(c, pos) -> if c == 'S' then ('|', pos) else cell)) rows
      else if x1 < sx && y2 < sy then
        map (map (\cell@(c, pos) -> if c == 'S' then ('J', pos) else cell)) rows
      else if x1 > sx && y2 < sy then
        map (map (\cell@(c, pos) -> if c == 'S' then ('L', pos) else cell)) rows
      else if x1 > sx && y2 > sy then
        map (map (\cell@(c, pos) -> if c == 'S' then ('F', pos) else cell)) rows
      else
        map (map (\cell@(c, pos) -> if c == 'S' then ('7', pos) else cell)) rows


isCorner :: Char -> Bool
isCorner c = c == 'F' || c == '7' || c == 'L' || c == 'J'

hcompatible :: Char -> Char -> Bool
hcompatible 'F' '7' = True
hcompatible '7' 'F' = True
hcompatible 'L' 'J' = True
hcompatible 'J' 'L' = True
hcompatible _ _ = False

vcompatible :: Char -> Char -> Bool
vcompatible 'F' 'L' = True
vcompatible 'L' 'F' = True
vcompatible 'J' '7' = True
vcompatible '7' 'J' = True
vcompatible _ _ = False

squeeze :: [[Cell]] -> Cell -> Set.Set Cell -> [Cell]
squeeze rows cur@(c, pos) visited
  | c == 'F' = adjacents (0, -1) (-1, 0) ( 1,  1)
  | c == '7' = adjacents (0, -1) ( 1, 0) (-1,  1)
  | c == 'L' = adjacents (0,  1) (-1, 0) ( 1, -1)
  | c == 'J' = adjacents (0,  1) ( 1, 0) (-1, -1)
  | otherwise = error "squeeze"
  where 
    adjacents (dxh, dyh) (dxv, dyv) (tx, ty) =
          map (\(_, (x, y)) -> rows `cellAt` (x+dxh, y+dyh)) (horizontals tx)
      ++  map (\(_, (x, y)) -> rows `cellAt` (x+dxv, y+dyv)) (verticals ty)
      ++  horizontals tx
      ++  verticals ty

    horizontals dx =
      let res = until
            ( \(cell@(c', _):_) -> isCorner c' && cell /= cur )
            ( \(cur@(_, (x, y)):cs) -> rows `cellAt` (x+dx, y) : cur : cs )
            [cur]
      in
        if (fst . head) res `hcompatible` fst cur then
          res
        else
          tail res
        

    verticals dy =
      let res = until
            ( \(cell@(c', _):_) -> isCorner c' && cell /= cur )
            ( \(cur@(_, (x, y)):cs) -> rows `cellAt` (x, y+dy) : cur : cs )
            [cur]
      in
        if (fst . head) res `vcompatible` fst cur then
          res
        else
          tail res

dfs :: [[Cell]] -> Set.Set Cell -> [[Cell]]
dfs rows path =
  let visited = doDfs [cellAt rows (0,0)] Set.empty in
    map (map (\cell@(c, pos) -> if cell `Set.member` visited && cell `Set.notMember` path then ('.', pos) else cell)) rows
  where
    nexts (x, y) =
      filter (
        \(x', y') ->
          x' >= 0 && x' <= (length . head) rows && y' >= 0 && y' <= length rows
        ) [(x, y-1), (x+1, y), (x, y+1), (x-1, y)]

    shouldVisit visited cell@(c, _) = cell `Set.notMember` visited && (cell `Set.notMember` path || isCorner c)

    isPath cell = cell `Set.member` path

    doDfs :: [Cell] -> Set.Set Cell -> Set.Set Cell
    doDfs [] visited = visited
    doDfs (cur:front) visited =
      let nextVisited = Set.insert cur visited in
        if isCorner (fst cur) then
          let squeezed = filter (shouldVisit visited) $
                if isCorner (fst cur) then squeeze rows cur visited else []
          in
            doDfs (squeezed ++ front) nextVisited
        else
          let pos = snd cur in
              let adjacent = map (cellAt rows) (nexts pos) in
                let nextCells = filter (shouldVisit visited) adjacent in
                  doDfs (nextCells ++ front) nextVisited

main :: IO ()
main = do
  lines <- lines <$> getContents

  let pss = zipWith zip lines coords
  let start = cellAt pss (findStart pss)

  let steps = traversePipes start pss
  let path = Set.fromList (map snd steps)

  let cleaned =  map (map (\cell@(_, pos) -> if cell `Set.notMember` path then (' ', pos) else cell)) pss
  let resolved = resolveSource cleaned start
  let pretty = map (map (\(c, _) -> convert c)) resolved

  let marked =  dfs resolved path
  let pretty2 = map (map (\(c, _) -> convert c)) marked

  mapM_ putStrLn pretty
  --mapM_ putStrLn pretty2
  
  -- part 1
  print $ (fst . head) steps

  -- part 2
  let ans2 = map (foldr (\(c, _) count -> if c == ' ' then count + 1 else count) 0) marked
  print $ sum ans2
