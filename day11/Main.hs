{-# LANGUAGE TupleSections #-}

import Data.List

type Pos = (Int, Int)
type Cell = (Char, Pos)

coords :: [[Pos]]
coords = map (\y -> map (, y) [0..]) [0..]

splitAtEmptyHorizontally :: [Cell] -> [Int] -> [[Cell]]
splitAtEmptyHorizontally cells empties =
  let pres = filter (\(_, (x, _))-> x < head empties) cells in
    let posts = filter (\(_, (x, _)) -> x > last empties) cells  in
      pres : zipWith (\lo hi -> filter (\(_, pos) -> fst pos > lo && fst pos < hi) cells) empties (tail empties)
      ++ [posts]

splitAtEmptyVertically :: [Cell] -> [Int] -> [[Cell]]
splitAtEmptyVertically cells empties =
  let pres = filter (\(_, (x, y)) -> y < head empties) cells in
    let posts = filter (\(_, (x, y)) -> y > last empties) cells in
      pres : zipWith (\lo hi -> filter (\(_, pos) -> snd pos > lo && snd pos < hi) cells) empties (tail empties)
      ++ [posts]

expand :: [Int] -> [Int] -> Int -> [Cell] -> [Cell]
expand emptyrows emptycols expandBy cells =
  let groupedHorizontally = zip offsets $ splitAtEmptyHorizontally cells emptycols in
    let expanded' = concatMap (\(offset, group) -> map (modify (offset, 0)) group) groupedHorizontally in
      let groupedVertically = zip offsets $ splitAtEmptyVertically expanded' emptyrows in
          concatMap (\(offset, group) -> map (modify (0, offset)) group) groupedVertically
  where
    offsets = map (* expandBy) [0..]

    modify (dx, dy) (c, (x, y)) = (c, (x+dx, y+dy))

distances :: [Cell] -> [Int]
distances cells = concatMap (\thisCell -> map (distance thisCell) cells) cells
  where
    distance (_, (x1, y1)) (_, (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  lines <- lines <$> getContents

  let emptyrows = map fst $ filter (\(_, line) -> '#' `notElem` line) (zip [0..] lines)
  let emptycols = map fst $ filter (\(_, line) -> '#' `notElem` line) (zip [0..] (transpose lines))

  let rows = zipWith zip lines coords
  let cells = concatMap (filter (\(c, _) -> c == '#')) rows 
  
  -- part 1
  let image1 = expand emptyrows emptycols 1 cells
  print $ sum (distances image1) `div` 2

  -- part 2
  let image2 = expand emptyrows emptycols 999999 cells
  print $ sum (distances image2) `div` 2
